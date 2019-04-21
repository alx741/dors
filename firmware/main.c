#include "eyes.h"
#include "motion.h"
#include <f1.h>
#include <rcc.h>
#include <usart.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

void delay(void);

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    command_init();
    eyes_init();
    motion_init();

    RCC_APB2ENR->IOPCEN = true;
    PORTC->MODE13 = MODE_OUTPUT_50MHZ;
    PORTC->CNF13 = CNF_OUT_PUSH_PULL;
    PORTC->ODR13 = true;

    command_loop();
}

void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
