#include "video.h"
#include "bitvec.h"
#include <f1.h>
#include <rcc.h>
#include <cmsis.h>
#include <stdint.h>
#include <string.h>

#define REFRESH_RATE_HZ  350

uint16_t FRAME_BUFFER[8] = {0};
uint16_t FRAME_BUFFER_SECONDARY[8] = {0};
static uint8_t CURRENT_ROW = 0;

void setup_timer(void);
void render_row(uint8_t index, uint16_t data);
void shift_push(uint8_t c);

void video_init(void)
{
    RCC_APB2ENR->IOPAEN = true;
    PORTA->MODE0 = MODE_OUTPUT_2MHZ;
    PORTA->CNF0 = CNF_OUT_PUSH_PULL;
    PORTA->MODE1 = MODE_OUTPUT_2MHZ;
    PORTA->CNF1 = CNF_OUT_PUSH_PULL;
    PORTA->MODE2 = MODE_OUTPUT_2MHZ;
    PORTA->CNF2 = CNF_OUT_PUSH_PULL;
    PORTA->MODE3 = MODE_OUTPUT_2MHZ;
    PORTA->CNF3 = CNF_OUT_PUSH_PULL;
    PORTA->ODR0 = false;
    PORTA->ODR1 = false;
    PORTA->ODR2 = false;
    PORTA->ODR3 = true;
    setup_timer();
}

void backup_frame_buffer(void)
{
    memcpy(FRAME_BUFFER_SECONDARY, FRAME_BUFFER, sizeof(FRAME_BUFFER));
}

void render()
{
    render_row(CURRENT_ROW, FRAME_BUFFER[CURRENT_ROW]);
    if (CURRENT_ROW == 7) { CURRENT_ROW = 0; }
    else { CURRENT_ROW++; }
}

void render_row(uint8_t index, uint16_t data)
{
    uint8_t i = 0b00000001 << index;
    uint8_t low = data & 0xFF;
    uint8_t high = (data >> 8) & 0xFF;
    shift_push(high);
    shift_push(~i);
    shift_push(low);
    shift_push(~i);
}

void shift_push(uint8_t c)
{
    PORTA->ODR3 = true; // Disable output
    PORTA->ODR0 = false;
    PORTA->ODR1 = false;
    PORTA->ODR2 = false;

    // Push data
    for (int i = 0; i < 8; i++)
    {
        if (c & (0x80 >> i))
        {
            PORTA->ODR0 = true; // Data
            PORTA->ODR2 = true; // Clock
            PORTA->ODR2 = false;
        }
        else
        {
            PORTA->ODR0 = false; // Data
            PORTA->ODR2 = true;  // Clock
            PORTA->ODR2 = false;
        }
    }

    // Latch

    PORTA->ODR1 = true;
    PORTA->ODR1 = false;
    PORTA->ODR3 = false; // Enable output
}

void setup_timer(void)
{
    __enable_irq();
    NVIC_EnableIRQ(TIM2_IRQ);
    NVIC_SetPriority(TIM2_IRQ, 1);
    RCC_APB1ENR->TIM2EN = true;
    TIM2_CR1->DIR = false; // Upcounter
    *TIM2_CNT = 0;
    *TIM2_PSC = 7200; // 10Khz
    *TIM2_ARR = ((1000 / REFRESH_RATE_HZ) * 10);
    TIM2_DIER->UIE = true; // TIM2 interrupt enable
    TIM2_CR1->CEN = true; // Enable counter
}

void TIM2_ISR(void)
{
    render();
    TIM2_SR->UIF = false;
}
