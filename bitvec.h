#pragma once
#include <stdbool.h>
#include <stdint.h>

// Memory efficient bit vector
typedef struct
{
    unsigned v0:1, v1:1, v2:1, v3:1, v4:1, v5:1, v6:1, v7:1;
} BIT_8VEC_t;

bool get_vec_bit(BIT_8VEC_t vec, uint8_t index);
