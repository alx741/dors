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
    RCC_APB1ENR->TIM3EN = true;
    RCC_APB2ENR->IOPAEN = true;
    RCC_APB2ENR->AFIOEN = true;
    PORTA->MODE6 = MODE_OUTPUT_50MHZ;
    PORTA->CNF6 = CNF_ALTERNATE_PUSH_PULL;

	TIM3_CR1->CKD  = 0b00; // No clock division
	TIM3_CR1->CMS  = 0b00; // Edge aligned mode
	TIM3_CR1->DIR  = false; // upcounter
	TIM3_CR1->OPM  = false; // continuous mode

    *TIM3_CNT  = 0;
    *TIM3_PSC  = 1000;
    *TIM3_ARR  = 10000;
    *TIM3_CCR1 = 1000;

    /* TIM3_CCMR1->CC2S = false; // Compare output */
    /* TIM3_CCMR1->OC2PE = true; // Enable preload register */
    /* TIM3_CCMR1->OC2M = 0b111; // PWM mode 2 */
    static volatile uint16_t* const ccmr1 = (void*) _TIM3 + 0x18;
    /* *ccmr1 = 0b0110100000000000; */
    *ccmr1 = 0b0000000001101000;

    TIM3_CR1->ARPE = true; // ARR is buffered

    TIM3_EGR->UG = true; // Update generation

    TIM3_CCER->CC1P = false; // Active high
    TIM3_CCER->CC1E = true; // Signal is output

    TIM3_CR1->CEN = true; // Counter enable

    while (true)
    {
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
