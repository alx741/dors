#include "motion.h"
#include <f1.h>
#include <rcc.h>
#include <stdbool.h>

#define PITCH_INIT_VAL 1100
#define YAW_INIT_VAL   1400

void motion_init()
{
    // 50Hz carrier; 1ms - 2ms duty cycle
    RCC_APB1ENR->TIM4EN = true;
    RCC_APB2ENR->IOPBEN = true;
    RCC_APB2ENR->AFIOEN = true;
    PORTB->MODE5 = MODE_OUTPUT_50MHZ;
    PORTB->CNF5 = CNF_OUT_PUSH_PULL;
    PORTB->MODE6 = MODE_OUTPUT_50MHZ;
    PORTB->CNF6 = CNF_ALTERNATE_PUSH_PULL;
    PORTB->MODE7 = MODE_OUTPUT_50MHZ;
    PORTB->CNF7 = CNF_ALTERNATE_PUSH_PULL;

    PORTB->ODR5 = true; // Deactivate yaw

    TIM4_CR1->CKD  = 0b00; // No clock division
    TIM4_CR1->CMS  = 0b00; // Edge aligned mode
    TIM4_CR1->DIR  = false; // Up counter
    TIM4_CR1->OPM  = false; // Continuous mode
    TIM4_CR1->ARPE = true; // ARR is buffered

    *TIM4_CNT  = 0;
    *TIM4_PSC  = 72; // 1MHz
    *TIM4_ARR  = 20000; // 50Hz
    *TIM4_CCR1 = PITCH_INIT_VAL; // 1000 - 2000
    *TIM4_CCR2 = YAW_INIT_VAL;   // 1000 - 2000

    TIM4_CCMR1->CC1S = false; // Compare output
    TIM4_CCMR1->CC2S = false;
    TIM4_CCMR1->OC1PE = true; // Enable preload register
    TIM4_CCMR1->OC2PE = true;
    TIM4_CCMR1->OC1M = 0b110; // PWM mode 1
    TIM4_CCMR1->OC2M = 0b110;

    TIM4_CCER->CC1P = false; // Active is high
    TIM4_CCER->CC2P = false;
    TIM4_CCER->CC1E = true; // Signal is output
    TIM4_CCER->CC2E = true;

    TIM4_EGR->UG = true; // Generate update event
    TIM4_CR1->CEN = true; // Counter enable
}

void step(DIRECTION_t d)
{
    switch (d)
    {
        case UP:
            step_up();
            break;
        case DOWN:
            step_down();
            break;
        case LEFT:
            step_left();
            break;
        case RIGHT:
            step_right();
            break;
    }
}

void step_left()
{
    int current = *TIM4_CCR2;
    if (current > 1000)
    {
        *TIM4_CCR2 = current - 30;
    }
}

void step_right()
{
    int current = *TIM4_CCR2;
    if (current < 1800)
    {
        *TIM4_CCR2 = current + 30;
    }
}

void step_down()
{
    int current = *TIM4_CCR1;
    if (current > 800)
    {
        *TIM4_CCR1 = current - 30;
    }
}

void step_up()
{
    int current = *TIM4_CCR1;
    if (current < 1600)
    {
        *TIM4_CCR1 = current + 30;
    }
}
