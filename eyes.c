#include "eyes.h"
#include <f1.h>
#include <rcc.h>

void shift_push(uint8_t c);

void eyes_init(void)
{
    RCC_APB2ENR->IOPCEN = true;

    // Eye 1 pins
    PORTC->MODE13 = MODE_OUTPUT_2MHZ;
    PORTC->CNF13 = CNF_OUT_PUSH_PULL;
    PORTC->MODE14 = MODE_OUTPUT_2MHZ;
    PORTC->CNF14 = CNF_OUT_PUSH_PULL;
    PORTC->MODE15 = MODE_OUTPUT_2MHZ;
    PORTC->CNF15 = CNF_OUT_PUSH_PULL;
    PORTC->ODR13 = false;
    PORTC->ODR14 = false;
    PORTC->ODR15 = false;

    // Eye 2 pins
    // ...
}

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
