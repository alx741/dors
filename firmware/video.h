#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

extern uint16_t FRAME_BUFFER[8];
extern uint16_t FRAME_BUFFER_BACKUP[8];

void video_init(void);
void backup_frame_buffer(void);
