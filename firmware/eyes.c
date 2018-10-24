#include "eyes.h"
#include "video.h"
#include <f1.h>
#include <rcc.h>
#include <cmsis.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

static uint8_t CURRENT_BLINK_PHASE = 0;
static bool BLINKING = false;

static EYE_t* CURRENT_LEFT_EYE;
static EYE_t* CURRENT_RIGHT_EYE;

void eyes_init(void)
{
    video_init();
    setup_blink_interval_timer();
}

void select_eyes(EYE_t eye_left, EYE_t eye_right)
{
    void *fb_ptr = &FRAME_BUFFER;

    CURRENT_LEFT_EYE  = eye_left;
    CURRENT_RIGHT_EYE = eye_right;

    for (int i = 0; i < 8; i++)
    {
        memset(fb_ptr++, vec2byte(eye_right[i]), 1);
        memset(fb_ptr++, vec2byte(eye_left[i]), 1);
    }
}

void blink(void)
{
    if (CURRENT_BLINK_PHASE == 0)
    {
        BLINKING = true;
        backup_frame_buffer();
        setup_blink_animation_timer();
    }
}

void blink_advance_phase(void)
{
    switch (CURRENT_BLINK_PHASE)
    {
        case 0:
            FRAME_BUFFER[0] = 0x0000;
            FRAME_BUFFER[7] = 0x0000;
            CURRENT_BLINK_PHASE++;
            break;

        case 1:
            FRAME_BUFFER[1] = 0x0000;
            FRAME_BUFFER[6] = 0x0000;
            CURRENT_BLINK_PHASE++;
            break;

        case 2:
            FRAME_BUFFER[2] = 0x0000;
            FRAME_BUFFER[5] = 0x0000;
            CURRENT_BLINK_PHASE++;
            break;

        case 3:
            FRAME_BUFFER[3] = 0xFFFF;
            FRAME_BUFFER[4] = 0xFFFF;
            CURRENT_BLINK_PHASE++;
            break;

        case 4:
            FRAME_BUFFER[3] = 0x0000;
            CURRENT_BLINK_PHASE++;
            break;

        case 5:
            FRAME_BUFFER[4] = FRAME_BUFFER_BACKUP[4];
            CURRENT_BLINK_PHASE++;
            break;

        case 6:
            FRAME_BUFFER[3] = FRAME_BUFFER_BACKUP[3];
            CURRENT_BLINK_PHASE++;
            break;

        case 7:
            FRAME_BUFFER[2] = FRAME_BUFFER_BACKUP[2];
            FRAME_BUFFER[5] = FRAME_BUFFER_BACKUP[5];
            CURRENT_BLINK_PHASE++;
            break;

        case 8:
            FRAME_BUFFER[1] = FRAME_BUFFER_BACKUP[1];
            FRAME_BUFFER[6] = FRAME_BUFFER_BACKUP[6];
            CURRENT_BLINK_PHASE++;
            break;

        case 9:
            FRAME_BUFFER[0] = FRAME_BUFFER_BACKUP[0];
            FRAME_BUFFER[7] = FRAME_BUFFER_BACKUP[7];
            TIM3_CR1->CEN = false; // Disable counter
            CURRENT_BLINK_PHASE = 0;
            BLINKING = false;
            setup_blink_interval_timer();
            break;
    }
}


void setup_blink_animation_timer(void)
{
    __enable_irq();
    NVIC_EnableIRQ(TIM3_IRQ);
    NVIC_SetPriority(TIM3_IRQ, 1);
    RCC_APB1ENR->TIM3EN = true;
    TIM3_CR1->DIR = false; // Upcounter
    *TIM3_CNT = 0;
    *TIM3_PSC = 7200; // 10Khz
    *TIM3_ARR = 200; // 50Hz
    TIM3_DIER->UIE = true; // TIM3 interrupt enable
    TIM3_CR1->CEN = true; // Enable counter
}

void setup_blink_interval_timer(void)
{
    int rand_interval = (rand() % (10 + 1 - 2)) + 2;
    __enable_irq();
    NVIC_EnableIRQ(TIM3_IRQ);
    NVIC_SetPriority(TIM3_IRQ, 1);
    RCC_APB1ENR->TIM3EN = true;
    TIM3_CR1->DIR = false; // Upcounter
    *TIM3_CNT = 0;
    *TIM3_PSC = 60000; // 1.2Khz
    *TIM3_ARR = rand_interval * 6000;
    TIM3_DIER->UIE = true; // TIM3 interrupt enable
    TIM3_CR1->CEN = true; // Enable counter
}

void TIM3_ISR(void)
{
    if (BLINKING)
    {
        blink_advance_phase();
    }
    else
    {
        TIM3_CR1->CEN = false; // Disable counter
        blink();
    }

    TIM3_SR->UIF = false;
}

const EYE_t eye_normal = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_surprised = {
      { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    };

const EYE_t eye_happy = {
      { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };


const EYE_t eye_smile = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,1,1,1,1,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,0,0,1,1,1 }
    , { 1,1,0,0,0,0,1,1 }
    , { 1,0,0,0,0,0,0,1 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_blink = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_squint = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };


const EYE_t eye_sleepy = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    };


const EYE_t eye_angry_left = {
      { 0,0,0,0,0,0,0,0 }
    , { 1,1,1,1,0,0,0,0 }
    , { 1,1,1,1,1,1,0,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    };


const EYE_t eye_angry_right = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,1,1,1,1 }
    , { 0,0,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,0 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    };


const EYE_t eye_confused_left = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,0 }
    , { 0,1,1,1,1,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };


const EYE_t eye_confused_right = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 1,1,1,1,1,1,0,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,0,1,1,1,1,1,1 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_sad_up_left = {
      { 0,0,0,0,1,1,1,0 }
    , { 0,0,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,0 }
    , { 0,1,1,1,1,1,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_sad_up_right = {
      { 0,1,1,1,0,0,0,0 }
    , { 1,1,1,1,1,1,0,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,1 }
    , { 0,0,1,1,1,1,1,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    };

const EYE_t eye_sad_down_left = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,1,1,1 }
    , { 0,0,0,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,0 }
    , { 1,1,1,1,1,1,0,0 }
    };

const EYE_t eye_sad_down_right = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 1,1,1,0,0,0,0,0 }
    , { 1,1,1,1,1,0,0,0 }
    , { 0,1,1,1,1,1,1,1 }
    , { 0,0,1,1,1,1,1,1 }
    };

const EYE_t eye_bored = {
      { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 0,0,0,0,0,0,0,0 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 0,1,1,1,1,1,1,0 }
    , { 0,0,1,1,1,1,0,0 }
    };


const EYE_t eye_full= {
      { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    , { 1,1,1,1,1,1,1,1 }
    };


const EYE_t eye_dead = {
      { 1,0,0,0,0,0,0,1 }
    , { 0,1,0,0,0,0,1,0 }
    , { 0,0,1,0,0,1,0,0 }
    , { 0,0,0,1,1,0,0,0 }
    , { 0,0,0,1,1,0,0,0 }
    , { 0,0,1,0,0,1,0,0 }
    , { 0,1,0,0,0,0,1,0 }
    , { 1,0,0,0,0,0,0,1 }
    };
