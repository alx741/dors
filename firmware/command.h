#pragma once
#include "emotion.h"
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

COMMAND_t receive_command();
void execute_command(COMMAND_t c);
