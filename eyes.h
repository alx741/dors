#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

typedef BIT_8VEC_t EYE_t[8];

void eyes_init(void);
void select_eyes(EYE_t eye_left, EYE_t eye_right);

/*
 * Available eyes
 */
const EYE_t eye_normal;
const EYE_t eye_happy;
const EYE_t eye_smile;
const EYE_t eye_squint;
const EYE_t eye_sleepy;
const EYE_t eye_bored;
const EYE_t eye_confused_left;
const EYE_t eye_confused_right;
const EYE_t eye_sad_up_left;
const EYE_t eye_sad_up_right;
const EYE_t eye_sad_down_left;
const EYE_t eye_sad_down_right;
const EYE_t eye_full;
const EYE_t eye_dead;
