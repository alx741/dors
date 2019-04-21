#include "command.h"
#include "emotion.h"
#include "motion.h"
#include <f1.h>
#include <stdbool.h>
#include <unistd.h>

#include "eyes.h"
#include "motion.h"

#define COMMAND_PROMPT '>'

typedef struct
{
    unsigned EMO_DIR : 4;
    unsigned         : 2;
    unsigned CMD     : 2;
} COMMAND_t;

/* Available commands */
#define SET_EYES    0x00
#define MOVE_HEAD   0x01
#define SET_EMOTION 0x02

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
    int c = getchar();
    return read_command(c);
    int c = 0b01010011;
    return read_command(c);
}

void execute_command(COMMAND_t c)
{
    PORTC->ODR13 ^= true;
    return;
}

void command_loop()
{
    int c;
    while (1)
    {
        putchar('\n');
        putchar(COMMAND_PROMPT);
        COMMAND_t cmd = receive_command();
        execute_command(cmd);
        c = getchar();

        if ((char) c == 'j')
        {
            step(DOWN);
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'k')
        {
            step(UP);
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'h')
        {
            step(LEFT);
            PORTC->ODR13 ^= true;
        }
        else if ((char) c == 'l')
        {
            step(RIGHT);
            PORTC->ODR13 ^= true;
        }

        else if ((char) c == 'H')
        {
            select_eyes(eye_happy, eye_happy);
        }

        else if ((char) c == 'n')
        {
            select_eyes(eye_normal, eye_normal);
        }

        else if ((char) c == 's')
        {
            select_eyes(eye_smile, eye_smile);
        }

        else if ((char) c == 'S')
        {
            select_eyes(eye_sad_up_left, eye_sad_up_right);
        }

        else if ((char) c == 'i')
        {
            PORTB->ODR5 ^= true;
        }

    }
}
