#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "bitvec.h"

typedef BIT_8VEC_t EYE_t[8];

void eyes_init(void);
void select_eyes(EYE_t *eye);

/*
 * Available eyes
 */
const EYE_t eye_default;
const EYE_t eye_happy;
