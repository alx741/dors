#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

extern uint16_t FRAME_BUFFER[8];

void video_init(void);

void shift_push(uint8_t c);
void render_row(uint8_t index, uint16_t data);
