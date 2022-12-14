'Program odczytu temperatury z czujnika DS18B20 i obsługi LCD oraz generatora PWM do zadania 3.
'Nie używane sa przerwania.
'by Marcin Kowalczyk
Const Prescfc = 1                                           'potęga dzielnika częstotliwości taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'częstotliwość po przeskalowaniu
Const _u2x0 = 0                                             'opcjonalne ustawienie bitu U2X0
Const Baundrs = 115200 / 2 ^ _u2x0                          'prędkość transmisji po RS [bps]
'Const _ubrr =(((fcrystal / Baundrs) / 16) - 1) 'na potem

$regfile = "m644pdef.dat"                                   'nazwa pliku konfiguracyjnego mikrokontrolera
$crystal = Fcrystal                                         'częstotliwość taktowania procesora
$baud = Baundrs                                             '115200
                                                   'prędkość transmisji dla instrukcji wysokiego poziomu jak typu Print

$programmer = 13                                            'wywołanie programatora z MCS bootloader
'Ustawiania

'ustawienie preskalera częstości taktowania mikrokontrolera
Clkpr = &H80                                                '&B10000000, aktywacja aktualizacji CLKPR
Clkpr = Prescfc                                             'wpisanie nowej wartości CLKPR

'Ucsr0a.u2x0 = _u2x0
#if _u2x0 = 1                                               'warunkowa kompilacja kodu
   SBI UCSR0A,U2X0                                          'podwojenie prędkości transmisji względem ustawienia w UBRR0
#endif


Config 1wire = Portc.2                                      'konfiguracja 1wire - przypisanie wyprowadzenia interfejsu

'konfiguracja LCD podłączonego bezpośrednio
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.5 , Db6 = Portb.6 , Db7 = Portb.7 , E = Portb.3 , Rs = Portb.2
Config Lcd = 16 * 2

'konfiguracja timer2 w trybie PWM z użyciem wyprowadzenia OC2A (PD.7)
Config Timer2 = Pwm , Prescale = 8 , Compare A Pwm = Clear Down , Compare B Pwm = Disconnect
Ocr2a = 64                                                  'wstępny współczynik DC = 64/256 = 0,25; wartość w rejestrze porównania timer2

Waitms 200                                                  'czas na stabilizację zasilania
Initlcd                                                     'wymuszenie opóźnionej inicjalizacji LCD

Cls                                                         'czyszczenie LCD, kursor na pozycję 1,1


'deklaracje zmiennych w SRAM: A-pomocniczy bajt do pętli i operacji odczytu 1wire; T - temperatura [st.C]
Dim A As Byte , T As Integer                                'deklaracja zmiennej byte w SRAM
Dim Ar(8) As Byte                                           'tabela 8 bajtów na kod czujnika


'jednorazowy odczyt kodu czujnika
1wreset                                                     'reset czujnika(ów)
1wwrite &H33                                                'read ROM command

For A = 1 To 8                                              'pętla for-next, A to adres w tabeli Ar()
   Ar(a) = 1wread()                                         'odczyt z czujnika bajtu do tabeli Ar() pod adresem A
Next

For A = 1 To 8
   Print Hex(ar(a)) ;                                       'transmisja RS tekstu kodu po konwersji HEX()
   Lcd Hex(ar(a))                                           'wydruk na LCD tekstu po konwersji HEX()
Next
Print                                                       'transmisja RS znaków CR i NL ( 13 i 10)

Locate 2 , 1                                                'ustawienie kursora w linii 2 na pozycji 1
Lcd "T[degC]="                                              'wydruk stałych znaków



'Pętla główna
Do                                                          'początek petli Do - Loop
   RCALL temp_conv                                          'przejście do procedury pod adresem etykiety: temp_conv
   Print T ; ", " ; Ocr2a                                   'transmisja tekstu po konwersji liczb DEC: T i rejestru generatora PWM
   Locate 2 , 9                                             'ustawienie pozycji kursora na początek wydruku temperatury
   Lcd T ; " "                                              'wydruk tekstu temperatury i 2. spacji do zamazania cyfr

   Waitms 400                                               'pauza 400 ms

   If Ucsr0a.rxc0 = 1 Then                                  'gdy bit RXC0 ustawiony - odebrany bajt w UDR0
      A = Udr0                                              'przepisanie bajtu do zmiennej A, wyczyszczenie RXC0
      If A = Asc( "i") Then Ocr2a = Ocr2a + 8               'gdy odebrano znak "i" - zwiększenie DC w gen. PWM
      If A = Asc( "d") Then Ocr2a = Ocr2a -8
      If A = Asc( "P") Then Start Watchdog                  'gdy odebran znak "P"wystartowanie watchdoga i restart uC
   End If                                                   'kończy działanie funkcji If
Loop


!temp_conv:                                                 'początek procedury konwersji i odczytu temperatury
'inicjalizacja konwersji temperatury
      1wreset
      1wwrite &H55                                          'polecenie porównania kodu dostępu
      For A = 1 To 8                                        'wysłanie 8 bajtów kodu do porównania przez czujnik
         1wwrite Ar(a)                                      'źródło bajtów w tabeli kodu
      Next
   1wwrite &H44                                             'polecenie inicjalizacji konwersji
   Waitms 100                                               'pauza 100ms na czas konwersji z rozdzialczością >9 bitów, wystarczy 8 bitów na część całkowitą temperatury

'odczyt temperatury i przetwarzanie wyniku do uzyskania tylko części całkowitej
      1wreset
      1wwrite &H55
      For A = 1 To 8
         1wwrite Ar(a)
      Next
   1wwrite &HBE                                             'polecenie zwrócenia zawartości pamięci czujnika

   A = 1wread()                                             'odczyt pierwszego bajtu pamięci - TempL - mniej znaczacy bajt typu integer
   'przepisanie zawartości spod adresu A pod adres mniej znaczącego bajtu TempL
   LDS r24,{a}                                              'załadowanie do rejestru procesora R24 spod adresu zmiennej A z SRAM
   STS {t},r24                                              'zachowanie zawartości R24 pod adresem zmiennej T w SRAM

   A = 1wread()                                             'odczyt bardziej znaczącego bajtu temperatury TempH - zawiera bit znaku temperatury
   LDS r24,{t}                                              'załadowanie do rejestru procesora R24 spod adresu zmiennej T mniej znaczącego bajtu TempL
   LDS r25,{a}                                              'załadowanie do rejestru procesora R25 spod adresu zmiennej A bardziej znaczącego bajtu TempH

   'podzielenie odczytanej z czujnika wartości Temp / 16 czyli 4x przesunięcie 16 bitów w prawo
   'w r25 TempH; w r24 TempL
   ASR r25                                                  'przesunięcie bitów w prawo tak, że na pozycję bitu 7 (znaku liczby) trafia bit jaki był przed przesunięciem; bit wychodzący trafia do C w SREG
   ROR r24                                                  'przesuniecie bitów w prawo tak, że na pozycję bitu 7 trafia bit C
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   STS {t},r24                                              'zachowanie mniej znaczącego bajtu temperatury TL pod adresem zmiennej T z offsetem +0 (bez offsetu)
   STS {t+1},r25                                            'zachowanie bardziej znaczącego bajtu temperatury TH pod adresem zmiennej T z offsetem +1
   RET                                                      'kończy procedurę - powrót do miejsca wyjścia -> za RCALL