#include "emotion.h"
#include "eyes.h"
#include "motion.h"

void convey_emotion(EMOTION_t e)
{
    switch (e)
    {
        case ANGRY:
            select_eyes(eye_angry_left, eye_angry_right);
            move_head(0.2, 0.5);
            break;

        case BORED:
            select_eyes(eye_bored, eye_bored);
            move_head(0.8, 0.8);
            break;

        case CONFUSED:
            select_eyes(eye_confused_left, eye_confused_right);
            move_head(0.3, 0.2);
            break;

        case HAPPY:
            select_eyes(eye_happy, eye_happy);
            move_head(0.8, 0.5);
            break;

        case NEUTRAL:
            select_eyes(eye_neutral, eye_neutral);
            move_head(0.5, 0.5);
            break;

        case SAD:
            select_eyes(eye_sad_up_left, eye_sad_up_right);
            move_head(0.0, 0.5);
            break;

        case SLEEPY:
            select_eyes(eye_sleepy, eye_sleepy);
            move_head(0.3, 0.5);
            break;

        case SMILEY:
            select_eyes(eye_smile, eye_smile);
            move_head(0.9, 0.5);
            break;

        case SURPRISED:
            select_eyes(eye_surprised, eye_surprised);
            move_head(0.6, 0.5);
            break;

        case SUSPICIOUS:
            select_eyes(eye_squint, eye_squint);
            move_head(0.4, 0.7);
            break;
    }
}
