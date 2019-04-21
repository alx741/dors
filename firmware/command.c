#include "command.h"
#include "emotion.h"
#include "motion.h"
#include <f1.h>
#include <usart.h>
#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>

#include "eyes.h"
#include "motion.h"


#define COMMAND_PROMPT '>'

/* Available commands */
#define SET_EYES    0x00
#define MOVE_HEAD   0x01
#define SET_EMOTION 0x02


typedef struct
{
    unsigned EMO_DIR : 4;
    unsigned         : 2;
    unsigned CMD     : 2;
} COMMAND_t;

void command_init()
{
    usart_init_72mhz_9600baud();
}

COMMAND_t read_command(int c)
{
    COMMAND_t cmd;
    cmd.CMD     = 0x03 & c;
    cmd.EMO_DIR = (0xF0 & c) >> 4;

#ifdef DEBUG
    printf("int: %x\n\n", c);
    printf("CMD: %x\n\n", cmd.CMD);
    printf("EMO_DIR: %x\n\n", cmd.EMO_DIR);
#endif

    return cmd;
}

COMMAND_t receive_command()
{
    return read_command(getchar());
}

void execute_command(COMMAND_t c)
{
    switch (c.CMD)
    {
        case SET_EYES:
            switch (c.EMO_DIR)
            {
                case NEUTRAL:
                select_eyes(eye_neutral, eye_neutral);
                break;

                case HAPPY:
                select_eyes(eye_happy, eye_happy);
                break;

                case SAD:
                select_eyes(eye_sad_up_left, eye_sad_up_right);
                break;
            }
            break;

    }

    PORTC->ODR13 ^= true;
    return;
}

void command_loop()
{
    while (1)
    {
        putchar(COMMAND_PROMPT);
        COMMAND_t cmd = receive_command();
        execute_command(cmd);
    }
}
