#include <f1.h>
#include <rcc.h>
#include <stdbool.h>
#include <stdio.h>

void delay(void);

void init_gpio(void)
{
    RCC_APB2ENR->IOPCEN = true;
    PORTC->MODE13 = MODE_OUTPUT_2MHZ;
    PORTC->CNF13 = CNF_OUT_PUSH_PULL;
    PORTC->MODE14 = MODE_OUTPUT_2MHZ;
    PORTC->CNF14 = CNF_OUT_PUSH_PULL;
    PORTC->MODE15 = MODE_OUTPUT_2MHZ;
    PORTC->CNF15 = CNF_OUT_PUSH_PULL;
    PORTC->ODR13 = false;
    PORTC->ODR14 = false;
    PORTC->ODR15 = false;
}

void shift_push(char c)
{
    PORTC->ODR13 = false;
    PORTC->ODR14 = false;
    PORTC->ODR15 = false;

    // Push data
    for (int i=0; i<8; i++)
    {
        if (c & (0x80 >> i))
        {
            PORTC->ODR13 = true; // Data
            PORTC->ODR15 = true; // Clock
            PORTC->ODR15 = false;
        }
        else
        {
            PORTC->ODR13 = false; // Data
            PORTC->ODR15 = true;  // Clock
            PORTC->ODR15 = false;
        }
    }

    // Latch
    PORTC->ODR14 = true;
    PORTC->ODR14 = false;
}

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    init_gpio();

    shift_push(0b00001001);

    while (true)
    {
    }
}

void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
