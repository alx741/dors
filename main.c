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

    while (true)
    {
        render_eye(eye_happy);
    }
}


void delay(void)
{
    for (int i = 0; i < 2000000; i++);
}
