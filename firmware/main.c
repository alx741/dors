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
    /* eyes_init(); */
    /* select_eyes(eye_happy, eye_happy); */

    // head control
    // SG90 - 50Hz carrier; 1ms - 2ms duty cycle
    RCC_APB1ENR->TIM2EN = true;
    RCC_APB2ENR->IOPAEN = true;
    RCC_APB2ENR->AFIOEN = true;
    PORTA->MODE1 = MODE_OUTPUT_50MHZ;
    PORTA->CNF1 = CNF_ALTERNATE_PUSH_PULL;

	TIM2_CR1->CKD  = 0b00; // No clock division
	TIM2_CR1->CMS  = 0b00; // Edge aligned mode
	TIM2_CR1->DIR  = false; // upcounter
	TIM2_CR1->OPM  = false; // continuous mode

    *TIM2_CNT  = 0;
    *TIM2_PSC  = 1000;
    *TIM2_ARR  = 10000;
    *TIM2_CCR2 = 1000;

    /* TIM2_CCMR1->CC2S = false; // Compare output */
    /* TIM2_CCMR1->OC2PE = true; // Enable preload register */
    /* TIM2_CCMR1->OC2M = 0b111; // PWM mode 2 */
    static volatile uint16_t* const ccmr1 = (void*) _TIM2 + 0x18;
    *ccmr1 = 0b0110100000000000;

    TIM2_CR1->ARPE = true; // ARR is buffered

    TIM2_EGR->UG = true; // Update generation

    TIM2_CCER->CC2P = false; // Active high
    TIM2_CCER->CC2E = true; // Signal is output

    TIM2_CR1->CEN = true; // Counter enable

    while (true)
    {
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
