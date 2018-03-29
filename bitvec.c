#include "bitvec.h"

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
