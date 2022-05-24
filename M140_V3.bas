'04-05-2022
'FUSES: E:FF H:DF L:E1

'Pinout ATtiny85:
'1- PB5 (PCINT5 / Reset / ADC0 / DW)
'2- PB3 (PCINT3/XTAL1/CLKI/OC1B/ADC3)
'3- PB4 (PCINT4/XTAL2/CLKO/_OC1B/ADC2)
'4- GND
'5- PB0 (MOSI/DI/SDA/AIN0/OC0A/_OC1A/AREF/PCINT0)
'6- PB1 (MISO/DO/AIN1/OC0B/OC1A/PCINT1)
'7- PB2 (SCK/USCK/SCL/ADC1/T0/INT0/PCINT2)
'8- VCC

'PB0 = SWITCH
'PB1 = LED
'PB2 = USB_charger
'PB3 = ADC3
'PB4 = PWM


$regfile = "attiny85.dat"
$crystal = 16000000
'$hwstack = 32
'$swstack = 10
'$framesize = 40

Dim Value As Word
Dim Ilaser As Word
Dim Cnt , Cnt2 As Word
Dim Pwm_value As Byte

Dim Pselect(4) As Byte                                      'Bascom não aceita arrays de constantes :/

'RSense =0.09ohms
'ADC Ref=1.1V
'Current=1.1V/0.09ohms =12.2(2)A
'LSb=12.2(2)/1024=0.011936V
'0.1A=0.1/ 0.011936V = ~8,4
'0.5A=0.5/ 0.011936V = ~41,9
'1.0A=1.0/ 0.011936V = ~83,8
'1.5A=1.5/ 0.011936V = ~125,7
Pselect(1) = 9 : Pselect(2) = 42 : Pselect(3) = 84 : Pselect(4) = 126       '0.1/0.5/1.0/1.5A


Dim Power_position , Power_output As Word                   'Tem de ser word Power_output

Config Adc = Single , Prescaler = 4 , Reference = Internal_1.1

Const Adc_ilaser = 3

Switch Alias Pinb.0
Led Alias Portb.1
Charger Alias Pinb.2

Config Charger = Input
Charger = 0
Config Led = Output
Led = 1
Config Switch = Input
Switch = 1

Declare Function Get_adc(byval Channel As Byte) As Word
Declare Sub Pwm(_get As Word , _wanted As Word)

'-----------------------------PLL AS CLOCK SOURCE for 16MHz-------------------------
Pllcsr.plle = 1
While Pllcsr.plock = 0
Wend
Pllcsr.pcke = 1
!Ldi R24 , 110
!Out Osccal , R24

'----------------------------TIMER 1 PWM
'TCCR1 – Timer/Counter1 Control Register
'CTC1 PWM1A COM1A1 COM1A0 CS13 CS12 CS11 CS10
Tccr1 = Bits(cs11 , Cs10 )
'GTCCR – General Timer/Counter1 Control Register
'TSM PWM1B COM1B1 COM1B0 FOC1B FOC1A PSR1 PSR0
Gtccr = Bits(pwm1b , Com1b1)
'TCNT1 – Timer/Counter1
Tcnt1 = &H00
Ocr1a = &H00
Ocr1b = &H00                                                'PWM VALUE
Ocr1c = &HFF
Config Portb.4 = Output

'Config Portb.1 = Output
'Open "comb.1:4800,8,n,1" For Output As #1

'------------------------------------------MAIN--------------------------------------------

Enable Interrupts

Power_position = 0
Power_output = Pselect(1)
Pwm_value = 0
Led = 0
Waitms 500
Led = 1

Do
    'Espera que o switch seja activado, pisca a cada segundo
    While Switch = 1

        'Verifica se o carregador está ligado
        While Charger = 1
            Led = 0
            For Cnt = 0 To 100
                If Charger = 0 Then Cnt = 100
                Waitms 5
            Next Cnt
            Led = 1
            For Cnt = 0 To 100
                If Charger = 0 Then Cnt = 100
                Waitms 5
            Next Cnt
        Wend

        For Cnt = 0 To 250
            If Switch = 0 Then Cnt = 250
            Waitms 4
        Next Cnt
        Led = 0
        Waitms 5
        Led = 1
    Wend

    'Se passado 150ms o switch continuar activado, então habilita o laser
    Waitms 150

    Led = 0
    While Switch = 0
        Ilaser = Get_adc(adc_ilaser)
        Call Pwm(ilaser , Power_output)
        Waitms 3                                            'soft start
    Wend
    Led = 1

    'Espera um possível 2º click para alterar a potência
    If Ocr1b = 0 Then
        For Cnt = 0 To 150
            Waitms 1
            If Switch = 0 Then
                Bitwait Switch , Set
                Incr Power_position
                Power_position = Power_position And &B11    '0..3
                Power_output = Pselect(power_position + 1)  'Arrays começam em 1
             'Apresenta a potência
                Waitms 500
                For Cnt2 = 0 To Power_position
                    Led = 0
                    Waitms 100
                    Led = 1
                    Waitms 200
                Next Cnt2
                Cnt = 150
            End If
        Next Cnt
    End If


    Ocr1b = 0                                               'desliga o pwm
    Pwm_value = 0
    Waitms 200
Loop

'---------------------------------------ADC-----------------------------------------------
Function Get_adc(byval Channel As Byte) As Word
    Dim Multi_sample As Byte
    Dim Adc_value As Word

    Adc_value = 0
    For Multi_sample = 0 To 49
        Adc_value = Adc_value + Getadc(channel)
    Next Multi_sample
    Adc_value = Adc_value / 50
    Get_adc = Adc_value
End Function

'-------------------------------------TUNE PWM------------------------------------------
Sub Pwm(_get As Word , _wanted As Word)
    If _get < _wanted And Pwm_value < 200 Then
        Incr Pwm_value
        Ocr1b = Pwm_value
    End If
    If _get > _wanted And Pwm_value > 0 Then
        Decr Pwm_value
        Ocr1b = Pwm_value
    End If
End Sub