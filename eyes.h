#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

typedef BIT_8VEC_t EYE_t[8];

void eyes_init(void);
/* void select_eyes(EYE_t eye); */
void select_eyes(EYE_t *eye);
void render_eyes();
/* bool get_eye_dot(const EYE_t eye, uint8_t row, uint8_t col); */
void render_row(uint8_t index, uint8_t data);

/*
 * Available eyes
 */
const EYE_t eye_default;
const EYE_t eye_happy;
