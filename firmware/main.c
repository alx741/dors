#include <f1.h>
#include <rcc.h>
#include <usart.h>
#include <stdbool.h>
#include <stdint.h>

#include <unistd.h>

#include <stdio.h>
#include "eyes.h"
#include "motion.h"

void delay(void);

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    usart_init_72mhz_9600baud();
    eyes_init();
    movement_init();

    select_eyes(eye_happy, eye_happy);

    RCC_APB2ENR->IOPCEN = true;
    PORTC->MODE13 = MODE_OUTPUT_50MHZ;
    PORTC->CNF13 = CNF_OUT_PUSH_PULL;
    PORTC->ODR13 = true;

    int c;
    while (true)
    {
        printf("pitch: %d\tyaw: %d\n\n>  ", *TIM4_CCR1, *TIM4_CCR2);
        c = getchar();

        if ((char) c == 'j')
        {
            step_down();
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'k')
        {
            step_up();
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'h')
        {
            step_left();
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'l')
        {
            step_right();
            PORTC->ODR13 ^= true;
        }
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
