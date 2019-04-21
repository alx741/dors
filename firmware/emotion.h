#pragma once
#include <stdint.h>

typedef uint8_t EMOTION_t;
void convey_emotion(EMOTION_t e);

// Emotions
#define ANGRY      0x01
#define BORED      0x02
#define CONFUSED   0x03
#define HAPPY      0x04
#define NEUTRAL    0x05
#define SAD        0x06
#define SLEEPY     0x07
#define SMILEY     0x08
#define SURPRISED  0x09
#define SUSPICIOUS 0x0A
