#include "bitvec.h"

uint8_t vec2byte(BIT_8VEC_t vec)
{
    uint8_t byte = 0;
    byte = byte | (vec.v0 << 0);
    byte = byte | (vec.v1 << 1);
    byte = byte | (vec.v2 << 2);
    byte = byte | (vec.v3 << 3);
    byte = byte | (vec.v4 << 4);
    byte = byte | (vec.v5 << 5);
    byte = byte | (vec.v6 << 6);
    byte = byte | (vec.v7 << 7);
    return byte;
}

inline bool get_vec_bit(BIT_8VEC_t vec, uint8_t index)
{
    switch (index)
    {
        case 0: return vec.v0; break;
        case 1: return vec.v1; break;
        case 2: return vec.v2; break;
        case 3: return vec.v3; break;
        case 4: return vec.v4; break;
        case 5: return vec.v5; break;
        case 6: return vec.v6; break;
        case 7: return vec.v7; break;
        default: return false;
    }
}
