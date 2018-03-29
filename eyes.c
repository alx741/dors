#include "eyes.h"
#include <f1.h>
#include <rcc.h>
#include <cmsis.h>
#include <string.h>

void shift_push(uint8_t c);
void sleep(void);
void setup_timer(void);

static EYE_t FRAME_BUFFER = {0};
static uint8_t CURRENT_RENDERING_ROW = 0;

void eyes_init(void)
{
    RCC_APB2ENR->IOPAEN = true;

    // Eye 1 pins
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

    // Eye 2 pins
    // ...

    setup_timer();
}

void setup_timer(void)
{
    __enable_irq();
    NVIC_EnableIRQ(TIM2_IRQ);
    NVIC_SetPriority(TIM2_IRQ, 1);
    RCC_APB1ENR->TIM2EN = true;
    TIM2_CR1->DIR = false; // Upcounter
    *TIM2_CNT = 0;
    *TIM2_PSC = 7200-1; // 10Khz
    *TIM2_ARR = 10-1; //
    TIM2_DIER->UIE = true; // TIM2 interrupt enable
    TIM2_CR1->CEN = true; // Enable counter
}

uint8_t get_eye_row(EYE_t eye, uint8_t row)
{
    return vec2byte(eye[row]);
}

bool get_eye_dot(EYE_t eye, uint8_t row, uint8_t col)
{
    return get_vec_bit(eye[row], col);
}

// TODO: use enum to select eyes
void select_eyes(EYE_t *eye)
{
    memcpy(&FRAME_BUFFER, eye, sizeof(EYE_t));
}

void render_eyes()
{
    render_row(CURRENT_RENDERING_ROW, get_eye_row(FRAME_BUFFER, CURRENT_RENDERING_ROW));
    if (CURRENT_RENDERING_ROW == 7)
    {
        CURRENT_RENDERING_ROW = 0;
    }
    else
    {
        CURRENT_RENDERING_ROW++;
    }
}

void render_row(uint8_t index, uint8_t data)
{
    uint8_t i = 0b00000001 << index;
    shift_push(data);
    shift_push(~i);
}

void shift_push(uint8_t c)
{
    PORTA->ODR3 = true; // Disable output
    PORTA->ODR0 = false;
    PORTA->ODR1 = false;
    PORTA->ODR2 = false;

    // Push data
    for (int i=0; i<8; i++)
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

void sleep(void)
{
    for (int i = 0; i < 1000; i++);
}

void TIM2_ISR(void)
{
    TIM2_SR->UIF = false;
    render_eyes();
}

const EYE_t eye_default = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    };


const EYE_t eye_happy = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,0,0,1,1,1 }
    , { 1,1,0,0,0,0,1,1 }
    , { 1,0,0,0,0,0,0,1 }
    , { 0,0,0,0,0,0,0,0 }
    };
