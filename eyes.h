#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

typedef BIT_8VEC_t EYE_t[8];

void eyes_init(void);
bool get_eye_dot(const EYE_t eye, uint8_t row, uint8_t col);

/*
 * Available eyes
 */
const EYE_t some_eye;
