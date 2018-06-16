#include <f1.h>
#include <rcc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "eyes.h"

void delay(void);

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    eyes_init();
    select_eyes(eye_happy, eye_happy);

    // head control
    // SG90 - 50Hz carrier; 1ms - 2ms duty cycle
    RCC_APB1ENR->TIM3EN = true;
    RCC_APB2ENR->IOPAEN = true;
    RCC_APB2ENR->AFIOEN = true;
    PORTA->MODE6 = MODE_OUTPUT_50MHZ;
    PORTA->CNF6 = CNF_ALTERNATE_PUSH_PULL;

	TIM3_CR1->CKD  = 0b00; // No clock division
	TIM3_CR1->CMS  = 0b00; // Edge aligned mode
	TIM3_CR1->DIR  = false; // Up counter
	TIM3_CR1->OPM  = false; // Continuous mode
    TIM3_CR1->ARPE = true; // ARR is buffered

    *TIM3_CNT  = 0;
    *TIM3_PSC  = 3600; // 10kHz
    *TIM3_ARR  = 200; // 50Hz
    *TIM3_CCR1 = 20; // 10 - 50

    TIM3_CCMR1->CC1S = false; // Compare output
    TIM3_CCMR1->OC1PE = true; // Enable preload register
    TIM3_CCMR1->OC1M = 0b110; // PWM mode 1

    TIM3_CCER->CC1P = false; // Active is high
    TIM3_CCER->CC1E = true; // Signal is output

    TIM3_EGR->UG = true; // Generate update event
    TIM3_CR1->CEN = true; // Counter enable

    while (true)
    {
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
