'Program odczytu temperatury z czujnika DS18B20 i obs?ugi LCD oraz generatora PWM do zadania 3.
'Nie u?ywane sa przerwania.
'by Marcin Kowalczyk
Const Prescfc = 1                                           'pot?ga dzielnika cz?stotliwo?ci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'cz?stotliwo?? po przeskalowaniu
Const _u2x0 = 0                                             'opcjonalne ustawienie bitu U2X0
Const Baundrs = 115200 / 2 ^ _u2x0                          'pr?dko?? transmisji po RS [bps]
'Const _ubrr =(((fcrystal / Baundrs) / 16) - 1) 'na potem

Const _fpr_adc = 1                                          'cz?stotliwo?? probkowania ADC 1s
Const _presc_timer_adc = 1024                               'dobierz tak, aby ocr1a nie przekracza?a zakresu licznika timera (timer1<2^16, timer0 i timer2 <256) i nie by?a u?amkiem
Const _ocr1a = Fcrystal / _presc_timer_adc / _fpr_adc -1    'oblicz i oce?

$regfile = "m644pdef.dat"                                   'nazwa pliku konfiguracyjnego mikrokontrolera
$crystal = Fcrystal                                         'cz?stotliwo?? taktowania procesora
$baud = Baundrs                                             '115200
                                                   'pr?dko?? transmisji dla instrukcji wysokiego poziomu jak typu Print

$programmer = 13                                            'wywo?anie programatora z MCS bootloader
'Ustawiania

'ustawienie preskalera cz?sto?ci taktowania mikrokontrolera
Clkpr = &H80                                                '&B10000000, aktywacja aktualizacji CLKPR
Clkpr = Prescfc                                             'wpisanie nowej warto?ci CLKPR

'Ucsr0a.u2x0 = _u2x0
#if _u2x0 = 1                                               'warunkowa kompilacja kodu
   SBI UCSR0A,U2X0                                          'podwojenie pr?dko?ci transmisji wzgl?dem ustawienia w UBRR0
#endif


Config 1wire = Portc.2                                      'konfiguracja 1wire - przypisanie wyprowadzenia interfejsu

'konfiguracja LCD pod??czonego bezpo?rednio
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.5 , Db6 = Portb.6 , Db7 = Portb.7 , E = Portb.3 , Rs = Portb.2
Config Lcd = 16 * 2

'konfiguracja timer2 w trybie PWM z u?yciem wyprowadzenia OC2A (PD.7)
Config Timer2 = Pwm , Prescale = 1 , Compare A Pwm = Clear Down , Compare B Pwm = Clear Down
Ocr2a = 168                                                 'wst?pny wsp??czynik DC = 64/256 = 0,25; warto?? w rejestrze por?wnania timer2
Ocr2b = 128

'konfiguracja ADC
Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc


Config Timer1 = Timer , Prescale = _presc_timer_adc , Compare A = Disconnect , Compare B = Disconnect , Clear Timer = 1
On Oc1a Init_adc Nosave
Stop Timer1
Timer1 = 0
Ocr1a = _ocr1a
Ocr1b = &HFFFF                                              'ocr1b > ocr1a, aby najpierw by?o przerwanie por?wnania ocr1a

Waitms 200                                                  'czas na stabilizacj? zasilania
Initlcd                                                     'wymuszenie op??nionej inicjalizacji LCD

Cls                                                         'czyszczenie LCD, kursor na pozycj? 1,1


'deklaracje zmiennych w SRAM: A-pomocniczy bajt do p?tli i operacji odczytu 1wire; T - temperatura [st.C]
Dim A As Byte , T As Integer                                'deklaracja zmiennej byte w SRAM
Dim Ar(8) As Byte                                           'tabela 8 bajt?w na kod czujnika
Dim W As Word                                               'warto?? po konwersji


'jednorazowy odczyt kodu czujnika
1wreset                                                     'reset czujnika(?w)
1wwrite &H33                                                'read ROM command

For A = 1 To 8                                              'p?tla for-next, A to adres w tabeli Ar()
   Ar(a) = 1wread()                                         'odczyt z czujnika bajtu do tabeli Ar() pod adresem A
Next

For A = 1 To 8
   Print Hex(ar(a)) ;                                       'transmisja RS tekstu kodu po konwersji HEX()
   Lcd Hex(ar(a))                                           'wydruk na LCD tekstu po konwersji HEX()
Next
Print                                                       'transmisja RS znak?w CR i NL ( 13 i 10)

Locate 2 , 1                                                'ustawienie kursora w linii 2 na pozycji 1
Lcd "W="                                                    'wydruk sta?ych znak?w

Enable Oc1a
SEI
Start Timer1

'P?tla g??wna
Do                                                          'pocz?tek petli Do - Loop
   If Portc.4 = 1 Then

      RCALL temp_conv                                       'przej?cie do procedury pod adresem etykiety: temp_conv

      W = Getadc(0)


      Print W ; ", " ; Ocr2b ; " " ; Bin(pina) ; " " ; Bin(ddrd)       'transmisja tekstu po konwersji liczb DEC: T i rejestru generatora PWM

      Locate 2 , 3                                          'ustawienie pozycji kursora na pocz?tek wydruku temperatury
      Lcd W ; "  " ; Hex(pina) ; " " ; Hex(ocr2b)           'wydruk tekstu temperatury i 2. spacji do zamazania cyfr

      'Waitms 400                                               'pauza 400 ms
      CBI PORTC,4
    End If







   If Ucsr0a.rxc0 = 1 Then                                  'gdy bit RXC0 ustawiony - odebrany bajt w UDR0
      A = Udr0                                              'przepisanie bajtu do zmiennej A, wyczyszczenie RXC0
      If A = Asc( "i") Then Incr Ocr2b                      'gdy odebrano znak "i" - zwi?kszenie DC w gen. PWM - OCR2B
      If A = Asc( "d") Then Decr Ocr2b

      If A = Asc( "w") Then Ocr2b = Ocr2b + 8               'gdy odebrano znak "w" - zwi?kszenie DC w gen. PWM
      If A = Asc( "z") Then Ocr2b = Ocr2b -8

      'If A = Asc( "s") Then Portd.6 = 1
      'If A = Asc( "c") Then Portd.6 = 0
      'If A = Asc( "o") Then Ddrd.6 = 1
      'If A = Asc( "a") Then Ddrd.6 = 0
      If A = Asc( "P") Then Start Watchdog                  'gdy odebran znak "P"wystartowanie watchdoga i restart uC
   End If                                                   'ko?czy dzia?anie funkcji If
Loop


!temp_conv:                                                 'pocz?tek procedury konwersji i odczytu temperatury
'inicjalizacja konwersji temperatury
      1wreset
      1wwrite &H55                                          'polecenie por?wnania kodu dost?pu
      For A = 1 To 8                                        'wys?anie 8 bajt?w kodu do por?wnania przez czujnik
         1wwrite Ar(a)                                      '?r?d?o bajt?w w tabeli kodu
      Next
   1wwrite &H44                                             'polecenie inicjalizacji konwersji
   Waitms 100                                               'pauza 100ms na czas konwersji z rozdzialczo?ci? >9 bit?w, wystarczy 8 bit?w na cz??? ca?kowit? temperatury

'odczyt temperatury i przetwarzanie wyniku do uzyskania tylko cz??ci ca?kowitej
      1wreset
      1wwrite &H55
      For A = 1 To 8
         1wwrite Ar(a)
      Next
   1wwrite &HBE                                             'polecenie zwr?cenia zawarto?ci pami?ci czujnika

   A = 1wread()                                             'odczyt pierwszego bajtu pami?ci - TempL - mniej znaczacy bajt typu integer
   'przepisanie zawarto?ci spod adresu A pod adres mniej znacz?cego bajtu TempL
   LDS r24,{a}                                              'za?adowanie do rejestru procesora R24 spod adresu zmiennej A z SRAM
   STS {t},r24                                              'zachowanie zawarto?ci R24 pod adresem zmiennej T w SRAM

   A = 1wread()                                             'odczyt bardziej znacz?cego bajtu temperatury TempH - zawiera bit znaku temperatury
   LDS r24,{t}                                              'za?adowanie do rejestru procesora R24 spod adresu zmiennej T mniej znacz?cego bajtu TempL
   LDS r25,{a}                                              'za?adowanie do rejestru procesora R25 spod adresu zmiennej A bardziej znacz?cego bajtu TempH

   'podzielenie odczytanej z czujnika warto?ci Temp / 16 czyli 4x przesuni?cie 16 bit?w w prawo
   'w r25 TempH; w r24 TempL
   ASR r25                                                  'przesuni?cie bit?w w prawo tak, ?e na pozycj? bitu 7 (znaku liczby) trafia bit jaki by? przed przesuni?ciem; bit wychodz?cy trafia do C w SREG
   ROR r24                                                  'przesuniecie bit?w w prawo tak, ?e na pozycj? bitu 7 trafia bit C
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   STS {t},r24                                              'zachowanie mniej znacz?cego bajtu temperatury TL pod adresem zmiennej T z offsetem +0 (bez offsetu)
   STS {t+1},r25                                            'zachowanie bardziej znacz?cego bajtu temperatury TH pod adresem zmiennej T z offsetem +1
RET                                                         'ko?czy procedur? - powr?t do miejsca wyj?cia -> za RCALL



Init_adc:
   SBI PORTC,4
Return