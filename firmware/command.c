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
#define SHUTDOWN    0x03


typedef struct
{
    unsigned YAW     : 4;
    unsigned PITCH   : 4;
    unsigned EMOTION : 4;
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
    cmd.CMD     = 0x0003  & c;
    cmd.EMOTION = (0x00F0 & c) >> 4;
    cmd.PITCH   = (0x0F00 & c) >> 8;
    cmd.YAW     = (0xF000 & c) >> 12;

#ifdef DEBUG
    printf("int cmd: \n%#018x\n\n", c);
    printf("CMD: %x\n\n", cmd.CMD);
    printf("EMOTION: %x\n\n", cmd.EMOTION);
    printf("PITCH: %x\n\n", cmd.PITCH);
    printf("YAW: %x\n\n", cmd.YAW);
#endif

    return cmd;
}

COMMAND_t receive_command()
{
    int command;
    int c1 = getchar();
    int c2 = getchar();
    command = 0xFF & c1;
    command |= (0xFF & c2) << 8;

#ifdef DEBUG
    printf("raw command: \n%#016x\n\n", command);
#endif

    return read_command(command);
}

void execute_command(COMMAND_t c)
{
    switch (c.CMD)
    {
        case SET_EYES:
            set_eyes_emotion(c.EMOTION);
            break;

        case MOVE_HEAD:
            move_head(c.PITCH/10.0, c.YAW/10.0);
            break;

        case SET_EMOTION:
            convey_emotion(c.EMOTION);
            break;

        case SHUTDOWN:
            move_head(0.0, 0.5);
            select_eyes(eye_empty, eye_empty);
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
