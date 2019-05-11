#pragma once
#include <stdint.h>

// Directions
#define UP    0x01
#define DOWN  0x02
#define LEFT  0x03
#define RIGHT 0x04

typedef uint8_t DIRECTION_t;

void motion_init(void);

/*
 * Position values in the continuous unit interval [0.0, 1.0]
 */
void move_head(float pitch, float yaw);

void step(DIRECTION_t d);
