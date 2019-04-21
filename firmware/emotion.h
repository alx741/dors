#pragma once
#include <stdint.h>

typedef uint8_t EMOTION_t;
void convey_emotion(EMOTION_t e);

// Emotions
#define NEUTRAL   0x01
#define HAPPY     0x02
#define SAD       0x03
#define SURPRISED 0x04
#define BORED     0x05
