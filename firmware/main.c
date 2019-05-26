#include "eyes.h"
#include "motion.h"
#include "emotion.h"
#include "command.h"
#include <f1.h>
#include <rcc.h>
#include <usart.h>
#include <stdbool.h>
#include <stdint.h>

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

    move_head(0.0, 0.5);
    select_eyes(eye_empty, eye_empty);

    command_loop();
}
