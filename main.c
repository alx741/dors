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

    /* shift_push(0b00000011); */
    /* shift_push(0b11111110); */
    PORTC->ODR13 = get_eye_dot(some_eye, 3, 3);

    while (true)
    {
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
