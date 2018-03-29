#include <f1.h>
#include <rcc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "bitvec.h"

void init_gpio(void);
void shift_push(uint8_t c);
void delay(void);

typedef BIT_8VEC_t EYE_t[8];

const EYE_t some_eye = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,0,0,0,0,0 }
    , { 0,0,0,1,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

bool get_eye_dot(const EYE_t eye, uint8_t row, uint8_t col)
{
    return get_vec_bit(eye[row], col);
}

/* void render_eye (EYE_t eye) */
/* { */
/*     for (int i=0; i<8; i++) */
/*     { */
/*     } */
/* } */

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    init_gpio();

    /* shift_push(0b00000011); */
    /* shift_push(0b11111110); */
    /* PORTC->ODR13 = get_eye_dot(some_eye, 3, 3); */

    while (true)
    {
    }
}

void shift_push(uint8_t c)
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

void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
