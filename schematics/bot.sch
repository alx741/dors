EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:switches
LIBS:relays
LIBS:motors
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:microcontrollers
LIBS:xilinx
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:MCU_ST_STM32F1
LIBS:bot-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L 74HC595 U?
U 1 1 5B12EF0E
P 3900 1800
F 0 "U?" H 4050 2400 50  0000 C CNN
F 1 "74HC595" H 3900 1200 50  0000 C CNN
F 2 "" H 3900 1800 50  0001 C CNN
F 3 "" H 3900 1800 50  0001 C CNN
	1    3900 1800
	-1   0    0    -1  
$EndComp
$Comp
L 74HC595 U?
U 1 1 5B12EFD5
P 3900 3250
F 0 "U?" H 4050 3850 50  0000 C CNN
F 1 "74HC595" H 3900 2650 50  0000 C CNN
F 2 "" H 3900 3250 50  0001 C CNN
F 3 "" H 3900 3250 50  0001 C CNN
	1    3900 3250
	-1   0    0    -1  
$EndComp
Wire Wire Line
	3200 2250 3200 2550
Wire Wire Line
	3200 2550 4600 2550
Wire Wire Line
	4600 2550 4600 2800
$Comp
L +3.3V #PWR?
U 1 1 5B12F1E6
P 4600 1650
F 0 "#PWR?" H 4600 1500 50  0001 C CNN
F 1 "+3.3V" H 4600 1790 50  0000 C CNN
F 2 "" H 4600 1650 50  0001 C CNN
F 3 "" H 4600 1650 50  0001 C CNN
	1    4600 1650
	0    1    -1   0   
$EndComp
$Comp
L +3.3V #PWR?
U 1 1 5B12F222
P 4600 3100
F 0 "#PWR?" H 4600 2950 50  0001 C CNN
F 1 "+3.3V" H 4600 3240 50  0000 C CNN
F 2 "" H 4600 3100 50  0001 C CNN
F 3 "" H 4600 3100 50  0001 C CNN
	1    4600 3100
	0    1    -1   0   
$EndComp
Wire Wire Line
	4600 1550 6950 1550
Wire Wire Line
	4850 1550 4850 6250
Wire Wire Line
	4850 3000 4600 3000
Wire Wire Line
	4600 1850 7000 1850
Wire Wire Line
	5000 1850 5000 6550
Wire Wire Line
	5000 3300 4600 3300
Wire Wire Line
	4600 1950 6850 1950
Wire Wire Line
	5150 1950 5150 6650
Wire Wire Line
	5150 3400 4600 3400
$Comp
L Conn_01x16_Male J?
U 1 1 5B12FAF9
P 1850 1950
F 0 "J?" H 1850 2750 50  0000 C CNN
F 1 "LED Matrix" H 1850 1050 50  0000 C CNN
F 2 "" H 1850 1950 50  0001 C CNN
F 3 "" H 1850 1950 50  0001 C CNN
	1    1850 1950
	1    0    0    1   
$EndComp
$Comp
L R R?
U 1 1 5B12FE45
P 2200 1150
F 0 "R?" V 2280 1150 50  0000 C CNN
F 1 "330" V 2200 1150 50  0000 C CNN
F 2 "" V 2130 1150 50  0001 C CNN
F 3 "" H 2200 1150 50  0001 C CNN
	1    2200 1150
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B12FF9A
P 2200 2450
F 0 "R?" V 2280 2450 50  0000 C CNN
F 1 "330" V 2200 2450 50  0000 C CNN
F 2 "" V 2130 2450 50  0001 C CNN
F 3 "" H 2200 2450 50  0001 C CNN
	1    2200 2450
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B130113
P 2200 2150
F 0 "R?" V 2280 2150 50  0000 C CNN
F 1 "330" V 2200 2150 50  0000 C CNN
F 2 "" V 2130 2150 50  0001 C CNN
F 3 "" H 2200 2150 50  0001 C CNN
	1    2200 2150
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B130193
P 2200 1750
F 0 "R?" V 2280 1750 50  0000 C CNN
F 1 "330" V 2200 1750 50  0000 C CNN
F 2 "" V 2130 1750 50  0001 C CNN
F 3 "" H 2200 1750 50  0001 C CNN
	1    2200 1750
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B130246
P 2200 1650
F 0 "R?" H 2280 1650 50  0000 C CNN
F 1 "330" V 2200 1650 50  0000 C CNN
F 2 "" V 2130 1650 50  0001 C CNN
F 3 "" H 2200 1650 50  0001 C CNN
	1    2200 1650
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B130303
P 2200 1450
F 0 "R?" V 2280 1450 50  0000 C CNN
F 1 "330" V 2200 1450 50  0000 C CNN
F 2 "" V 2130 1450 50  0001 C CNN
F 3 "" H 2200 1450 50  0001 C CNN
	1    2200 1450
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B13035F
P 2200 1250
F 0 "R?" V 2280 1250 50  0000 C CNN
F 1 "330" V 2200 1250 50  0000 C CNN
F 2 "" V 2130 1250 50  0001 C CNN
F 3 "" H 2200 1250 50  0001 C CNN
	1    2200 1250
	0    -1   1    0   
$EndComp
Wire Wire Line
	3200 1750 3000 1750
Wire Wire Line
	3000 1750 3000 2650
Wire Wire Line
	3000 2650 2050 2650
Wire Wire Line
	3200 1950 3100 1950
Wire Wire Line
	3100 1950 3100 2550
Wire Wire Line
	3100 2550 2050 2550
Wire Wire Line
	3200 2900 3050 2900
Wire Wire Line
	3050 2900 3050 2450
Wire Wire Line
	3050 2450 2350 2450
Wire Wire Line
	3200 3000 2950 3000
Wire Wire Line
	2950 3000 2950 2350
Wire Wire Line
	2950 2350 2050 2350
Wire Wire Line
	2050 2250 3050 2250
Wire Wire Line
	3050 2250 3050 2050
Wire Wire Line
	3050 2050 3200 2050
Wire Wire Line
	2350 2150 2900 2150
Wire Wire Line
	2900 2150 2900 3200
Wire Wire Line
	2900 3200 3200 3200
Wire Wire Line
	2050 2050 2950 2050
Wire Wire Line
	2950 2050 2950 1850
Wire Wire Line
	2950 1850 3200 1850
Wire Wire Line
	2050 1950 2900 1950
Wire Wire Line
	2900 1950 2900 1550
Wire Wire Line
	2900 1550 3200 1550
Wire Wire Line
	2050 1850 2850 1850
Wire Wire Line
	2850 1850 2850 1350
Wire Wire Line
	2850 1350 3200 1350
Wire Wire Line
	2350 1750 2800 1750
Wire Wire Line
	2800 1750 2800 3100
Wire Wire Line
	2800 3100 3200 3100
Wire Wire Line
	2350 1650 2750 1650
Wire Wire Line
	2750 1650 2750 3300
Wire Wire Line
	2750 3300 3200 3300
Wire Wire Line
	2050 1550 2800 1550
Wire Wire Line
	2800 1550 2800 1650
Wire Wire Line
	2800 1650 3200 1650
Wire Wire Line
	2350 1450 2700 1450
Wire Wire Line
	2700 1450 2700 2800
Wire Wire Line
	2700 2800 3200 2800
Wire Wire Line
	2050 1350 2800 1350
Wire Wire Line
	2800 1350 2800 1450
Wire Wire Line
	2800 1450 3200 1450
Wire Wire Line
	2350 1250 2650 1250
Wire Wire Line
	2650 1250 2650 3400
Wire Wire Line
	2650 3400 3200 3400
Wire Wire Line
	2350 1150 2600 1150
Wire Wire Line
	2600 1150 2600 3500
Wire Wire Line
	2600 3500 3200 3500
$Comp
L 74HC595 U?
U 1 1 5B132614
P 3900 5050
F 0 "U?" H 4050 5650 50  0000 C CNN
F 1 "74HC595" H 3900 4450 50  0000 C CNN
F 2 "" H 3900 5050 50  0001 C CNN
F 3 "" H 3900 5050 50  0001 C CNN
	1    3900 5050
	-1   0    0    -1  
$EndComp
$Comp
L 74HC595 U?
U 1 1 5B13261A
P 3900 6500
F 0 "U?" H 4050 7100 50  0000 C CNN
F 1 "74HC595" H 3900 5900 50  0000 C CNN
F 2 "" H 3900 6500 50  0001 C CNN
F 3 "" H 3900 6500 50  0001 C CNN
	1    3900 6500
	-1   0    0    -1  
$EndComp
Wire Wire Line
	3200 5500 3200 5800
Wire Wire Line
	3200 5800 4600 5800
Wire Wire Line
	4600 5800 4600 6050
$Comp
L +3.3V #PWR?
U 1 1 5B132623
P 4600 4900
F 0 "#PWR?" H 4600 4750 50  0001 C CNN
F 1 "+3.3V" H 4600 5040 50  0000 C CNN
F 2 "" H 4600 4900 50  0001 C CNN
F 3 "" H 4600 4900 50  0001 C CNN
	1    4600 4900
	0    1    -1   0   
$EndComp
$Comp
L +3.3V #PWR?
U 1 1 5B132629
P 4600 6350
F 0 "#PWR?" H 4600 6200 50  0001 C CNN
F 1 "+3.3V" H 4600 6490 50  0000 C CNN
F 2 "" H 4600 6350 50  0001 C CNN
F 3 "" H 4600 6350 50  0001 C CNN
	1    4600 6350
	0    1    -1   0   
$EndComp
Wire Wire Line
	4850 4800 4600 4800
Wire Wire Line
	4850 6250 4600 6250
Wire Wire Line
	5000 5100 4600 5100
Wire Wire Line
	5000 6550 4600 6550
Wire Wire Line
	5150 5200 4600 5200
Wire Wire Line
	5150 6650 4600 6650
$Comp
L Conn_01x16_Male J?
U 1 1 5B132638
P 1850 5200
F 0 "J?" H 1850 6000 50  0000 C CNN
F 1 "LED Matrix" H 1850 4300 50  0000 C CNN
F 2 "" H 1850 5200 50  0001 C CNN
F 3 "" H 1850 5200 50  0001 C CNN
	1    1850 5200
	1    0    0    1   
$EndComp
$Comp
L R R?
U 1 1 5B13263E
P 2200 4400
F 0 "R?" V 2280 4400 50  0000 C CNN
F 1 "330" V 2200 4400 50  0000 C CNN
F 2 "" V 2130 4400 50  0001 C CNN
F 3 "" H 2200 4400 50  0001 C CNN
	1    2200 4400
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B132644
P 2200 5700
F 0 "R?" V 2280 5700 50  0000 C CNN
F 1 "330" V 2200 5700 50  0000 C CNN
F 2 "" V 2130 5700 50  0001 C CNN
F 3 "" H 2200 5700 50  0001 C CNN
	1    2200 5700
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B13264A
P 2200 5400
F 0 "R?" V 2280 5400 50  0000 C CNN
F 1 "330" V 2200 5400 50  0000 C CNN
F 2 "" V 2130 5400 50  0001 C CNN
F 3 "" H 2200 5400 50  0001 C CNN
	1    2200 5400
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B132650
P 2200 5000
F 0 "R?" V 2280 5000 50  0000 C CNN
F 1 "330" V 2200 5000 50  0000 C CNN
F 2 "" V 2130 5000 50  0001 C CNN
F 3 "" H 2200 5000 50  0001 C CNN
	1    2200 5000
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B132656
P 2200 4900
F 0 "R?" H 2280 4900 50  0000 C CNN
F 1 "330" V 2200 4900 50  0000 C CNN
F 2 "" V 2130 4900 50  0001 C CNN
F 3 "" H 2200 4900 50  0001 C CNN
	1    2200 4900
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B13265C
P 2200 4700
F 0 "R?" V 2280 4700 50  0000 C CNN
F 1 "330" V 2200 4700 50  0000 C CNN
F 2 "" V 2130 4700 50  0001 C CNN
F 3 "" H 2200 4700 50  0001 C CNN
	1    2200 4700
	0    -1   1    0   
$EndComp
$Comp
L R R?
U 1 1 5B132662
P 2200 4500
F 0 "R?" V 2280 4500 50  0000 C CNN
F 1 "330" V 2200 4500 50  0000 C CNN
F 2 "" V 2130 4500 50  0001 C CNN
F 3 "" H 2200 4500 50  0001 C CNN
	1    2200 4500
	0    -1   1    0   
$EndComp
Wire Wire Line
	3200 5000 3000 5000
Wire Wire Line
	3000 5000 3000 5900
Wire Wire Line
	3000 5900 2050 5900
Wire Wire Line
	3200 5200 3100 5200
Wire Wire Line
	3100 5200 3100 5800
Wire Wire Line
	3100 5800 2050 5800
Wire Wire Line
	3200 6150 3050 6150
Wire Wire Line
	3050 6150 3050 5700
Wire Wire Line
	3050 5700 2350 5700
Wire Wire Line
	3200 6250 2950 6250
Wire Wire Line
	2950 6250 2950 5600
Wire Wire Line
	2950 5600 2050 5600
Wire Wire Line
	2050 5500 3050 5500
Wire Wire Line
	3050 5500 3050 5300
Wire Wire Line
	3050 5300 3200 5300
Wire Wire Line
	2350 5400 2900 5400
Wire Wire Line
	2900 5400 2900 6450
Wire Wire Line
	2900 6450 3200 6450
Wire Wire Line
	2050 5300 2950 5300
Wire Wire Line
	2950 5300 2950 5100
Wire Wire Line
	2950 5100 3200 5100
Wire Wire Line
	2050 5200 2900 5200
Wire Wire Line
	2900 5200 2900 4800
Wire Wire Line
	2900 4800 3200 4800
Wire Wire Line
	2050 5100 2850 5100
Wire Wire Line
	2850 5100 2850 4600
Wire Wire Line
	2850 4600 3200 4600
Wire Wire Line
	2350 5000 2800 5000
Wire Wire Line
	2800 5000 2800 6350
Wire Wire Line
	2800 6350 3200 6350
Wire Wire Line
	2350 4900 2750 4900
Wire Wire Line
	2750 4900 2750 6550
Wire Wire Line
	2750 6550 3200 6550
Wire Wire Line
	2050 4800 2800 4800
Wire Wire Line
	2800 4800 2800 4900
Wire Wire Line
	2800 4900 3200 4900
Wire Wire Line
	2350 4700 2700 4700
Wire Wire Line
	2700 4700 2700 6050
Wire Wire Line
	2700 6050 3200 6050
Wire Wire Line
	2050 4600 2800 4600
Wire Wire Line
	2800 4600 2800 4700
Wire Wire Line
	2800 4700 3200 4700
Wire Wire Line
	2350 4500 2650 4500
Wire Wire Line
	2650 4500 2650 6650
Wire Wire Line
	2650 6650 3200 6650
Wire Wire Line
	2350 4400 2600 4400
Wire Wire Line
	2600 4400 2600 6750
Wire Wire Line
	2600 6750 3200 6750
Wire Wire Line
	3200 3700 3200 4250
Wire Wire Line
	3200 4250 4600 4250
Wire Wire Line
	4600 4250 4600 4600
Connection ~ 4850 4800
Connection ~ 4850 3000
Connection ~ 5000 5100
Connection ~ 5000 3300
Connection ~ 5150 5200
Connection ~ 5150 3400
$Comp
L STM32F103C8Tx U?
U 1 1 5B1300F7
P 7800 3350
F 0 "U?" H 7200 4800 50  0000 L CNN
F 1 "STM32F103C8Tx" H 8100 4800 50  0000 L CNN
F 2 "Package_QFP:LQFP-48_7x7mm_P0.5mm" H 7200 1950 50  0001 R CNN
F 3 "" H 7800 3350 50  0001 C CNN
	1    7800 3350
	-1   0    0    1   
$EndComp
$Comp
L +3.3V #PWR?
U 1 1 5B130C16
P 7700 1850
F 0 "#PWR?" H 7700 1700 50  0001 C CNN
F 1 "+3.3V" H 7700 1990 50  0000 C CNN
F 2 "" H 7700 1850 50  0001 C CNN
F 3 "" H 7700 1850 50  0001 C CNN
	1    7700 1850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 5B13112B
P 7600 4850
F 0 "#PWR?" H 7600 4600 50  0001 C CNN
F 1 "GND" H 7600 4700 50  0000 C CNN
F 2 "" H 7600 4850 50  0001 C CNN
F 3 "" H 7600 4850 50  0001 C CNN
	1    7600 4850
	1    0    0    -1  
$EndComp
Wire Wire Line
	7200 3250 6850 3250
Wire Wire Line
	6850 3250 6850 1950
Connection ~ 5150 1950
Wire Wire Line
	7200 3550 7100 3550
Wire Wire Line
	7100 3550 7100 1350
Wire Wire Line
	7100 1350 4600 1350
Wire Wire Line
	7200 3350 6950 3350
Wire Wire Line
	6950 3350 6950 1550
Connection ~ 4850 1550
Wire Wire Line
	7200 3450 7000 3450
Wire Wire Line
	7000 3450 7000 1850
Connection ~ 5000 1850
$EndSCHEMATC