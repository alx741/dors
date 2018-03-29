#include "eyes.h"
#include "video.h"
#include <f1.h>
#include <rcc.h>
#include <cmsis.h>
#include <string.h>

void eyes_init(void)
{
    video_init();
}

// TODO: use enum to select eyes
void select_eyes(EYE_t *eye)
{
    memcpy(&FRAME_BUFFER, eye, sizeof(EYE_t));
}

const EYE_t eye_default = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    };


const EYE_t eye_happy = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,0,0,1,1,1 }
    , { 1,1,0,0,0,0,1,1 }
    , { 1,0,0,0,0,0,0,1 }
    , { 0,0,0,0,0,0,0,0 }
    };
