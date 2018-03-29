#include <f1.h>
#include <rcc.h>
#include <stdbool.h>
#include <stdio.h>

void init_gpio(void);
void shift_push(uint8_t c);
void delay(void);

// Memory efficient 8x8 bit matrix
typedef struct
{
    unsigned row1_col1 : 1, row1_col2 : 1, row1_col3 : 1, row1_col4 : 1, row1_col5 : 1, row1_col6 : 1, row1_col7 : 1, row1_col8 : 1;
    unsigned row2_col1 : 1, row2_col2 : 1, row2_col3 : 1, row2_col4 : 1, row2_col5 : 1, row2_col6 : 1, row2_col7 : 1, row2_col8 : 1;
    unsigned row3_col1 : 1, row3_col2 : 1, row3_col3 : 1, row3_col4 : 1, row3_col5 : 1, row3_col6 : 1, row3_col7 : 1, row3_col8 : 1;
    unsigned row4_col1 : 1, row4_col2 : 1, row4_col3 : 1, row4_col4 : 1, row4_col5 : 1, row4_col6 : 1, row4_col7 : 1, row4_col8 : 1;
    unsigned row5_col1 : 1, row5_col2 : 1, row5_col3 : 1, row5_col4 : 1, row5_col5 : 1, row5_col6 : 1, row5_col7 : 1, row5_col8 : 1;
    unsigned row6_col1 : 1, row6_col2 : 1, row6_col3 : 1, row6_col4 : 1, row6_col5 : 1, row6_col6 : 1, row6_col7 : 1, row6_col8 : 1;
    unsigned row7_col1 : 1, row7_col2 : 1, row7_col3 : 1, row7_col4 : 1, row7_col5 : 1, row7_col6 : 1, row7_col7 : 1, row7_col8 : 1;
    unsigned row8_col1 : 1, row8_col2 : 1, row8_col3 : 1, row8_col4 : 1, row8_col5 : 1, row8_col6 : 1, row8_col7 : 1, row8_col8 : 1;
} EYE_t;


const EYE_t some_eye =
    { 0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0
    , 0,0,1,0,0,0,0,0
    , 0,0,0,1,0,0,0,0
    , 0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0
    };

int main(void)
{
    rcc_setup_in_8mhz_hse_out_72mhz();
    init_gpio();

    shift_push(0b00000011);
    shift_push(0b11111110);

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
