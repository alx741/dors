#include "command.h"
#include "emotion.h"
#include "eyes.h"
#include "motion.h"
#include <f1.h>
#include <usart.h>
#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>


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
            set_eyes_emotion(c.EMO_DIR);
            break;

        case MOVE_HEAD:
            step(c.EMO_DIR);
            break;

        case SET_EMOTION:
            // TODO
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

void set_eyes_emotion(uint8_t emotion)
{
    switch (emotion)
    {
        case ANGRY:
        select_eyes(eye_angry_left, eye_angry_right);
        break;

        case BORED:
        select_eyes(eye_bored, eye_bored);
        break;

        case CONFUSED:
        select_eyes(eye_confused_left, eye_confused_right);
        break;

        case HAPPY:
        select_eyes(eye_happy, eye_happy);
        break;

        case NEUTRAL:
        select_eyes(eye_neutral, eye_neutral);
        break;

        case SAD:
        select_eyes(eye_sad_up_left, eye_sad_up_right);
        break;

        case SLEEPY:
        select_eyes(eye_sleepy, eye_sleepy);
        break;

        case SMILEY:
        select_eyes(eye_smile, eye_smile);
        break;

        case SURPRISED:
        select_eyes(eye_surprised, eye_surprised);
        break;

        case SUSPICIOUS:
        select_eyes(eye_squint, eye_squint);
        break;
    }
}
