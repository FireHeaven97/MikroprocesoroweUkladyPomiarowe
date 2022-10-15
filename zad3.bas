'Program odczytu temperatury z czujnika DS18B20 i obs³ugi LCD oraz generatora PWM do zadania 3.
'Nie u¿ywane sa przerwania.
'by Marcin Kowalczyk
Const Prescfc = 1                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
Const _u2x0 = 0                                             'opcjonalne ustawienie bitu U2X0
Const Baundrs = 115200 / 2 ^ _u2x0                          'prêdkoœæ transmisji po RS [bps]
'Const _ubrr =(((fcrystal / Baundrs) / 16) - 1) 'na potem

$regfile = "m644pdef.dat"                                   'nazwa pliku konfiguracyjnego mikrokontrolera
$crystal = Fcrystal                                         'czêstotliwoœæ taktowania procesora
$baud = Baundrs                                             '115200
                                                   'prêdkoœæ transmisji dla instrukcji wysokiego poziomu jak typu Print

$programmer = 13                                            'wywo³anie programatora z MCS bootloader
'Ustawiania

'ustawienie preskalera czêstoœci taktowania mikrokontrolera
Clkpr = &H80                                                '&B10000000, aktywacja aktualizacji CLKPR
Clkpr = Prescfc                                             'wpisanie nowej wartoœci CLKPR

'Ucsr0a.u2x0 = _u2x0
#if _u2x0 = 1                                               'warunkowa kompilacja kodu
   SBI UCSR0A,U2X0                                          'podwojenie prêdkoœci transmisji wzglêdem ustawienia w UBRR0
#endif


Config 1wire = Portc.2                                      'konfiguracja 1wire - przypisanie wyprowadzenia interfejsu

'konfiguracja LCD pod³¹czonego bezpoœrednio
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.5 , Db6 = Portb.6 , Db7 = Portb.7 , E = Portb.3 , Rs = Portb.2
Config Lcd = 16 * 2

'konfiguracja timer2 w trybie PWM z u¿yciem wyprowadzenia OC2A (PD.7)
Config Timer2 = Pwm , Prescale = 8 , Compare A Pwm = Clear Down , Compare B Pwm = Disconnect
Ocr2a = 64                                                  'wstêpny wspó³czynik DC = 64/256 = 0,25; wartoœæ w rejestrze porównania timer2

Waitms 200                                                  'czas na stabilizacjê zasilania
Initlcd                                                     'wymuszenie opóŸnionej inicjalizacji LCD

Cls                                                         'czyszczenie LCD, kursor na pozycjê 1,1


'deklaracje zmiennych w SRAM: A-pomocniczy bajt do pêtli i operacji odczytu 1wire; T - temperatura [st.C]
Dim A As Byte , T As Integer                                'deklaracja zmiennej byte w SRAM
Dim Ar(8) As Byte                                           'tabela 8 bajtów na kod czujnika


'jednorazowy odczyt kodu czujnika
1wreset                                                     'reset czujnika(ów)
1wwrite &H33                                                'read ROM command

For A = 1 To 8                                              'pêtla for-next, A to adres w tabeli Ar()
   Ar(a) = 1wread()                                         'odczyt z czujnika bajtu do tabeli Ar() pod adresem A
Next

For A = 1 To 8
   Print Hex(ar(a)) ;                                       'transmisja RS tekstu kodu po konwersji HEX()
   Lcd Hex(ar(a))                                           'wydruk na LCD tekstu po konwersji HEX()
Next
Print                                                       'transmisja RS znaków CR i NL ( 13 i 10)

Locate 2 , 1                                                'ustawienie kursora w linii 2 na pozycji 1
Lcd "T[degC]="                                              'wydruk sta³ych znaków



'Pêtla g³ówna
Do                                                          'pocz¹tek petli Do - Loop
   RCALL temp_conv                                          'przejœcie do procedury pod adresem etykiety: temp_conv
   Print T ; ", " ; Ocr2a                                   'transmisja tekstu po konwersji liczb DEC: T i rejestru generatora PWM
   Locate 2 , 9                                             'ustawienie pozycji kursora na pocz¹tek wydruku temperatury
   Lcd T ; " "                                              'wydruk tekstu temperatury i 2. spacji do zamazania cyfr

   Waitms 400                                               'pauza 400 ms

   If Ucsr0a.rxc0 = 1 Then                                  'gdy bit RXC0 ustawiony - odebrany bajt w UDR0
      A = Udr0                                              'przepisanie bajtu do zmiennej A, wyczyszczenie RXC0
      If A = Asc( "i") Then Ocr2a = Ocr2a + 8               'gdy odebrano znak "i" - zwiêkszenie DC w gen. PWM
      If A = Asc( "d") Then Ocr2a = Ocr2a -8
      If A = Asc( "P") Then Start Watchdog                  'gdy odebran znak "P"wystartowanie watchdoga i restart uC
   End If                                                   'koñczy dzia³anie funkcji If
Loop


!temp_conv:                                                 'pocz¹tek procedury konwersji i odczytu temperatury
'inicjalizacja konwersji temperatury
      1wreset
      1wwrite &H55                                          'polecenie porównania kodu dostêpu
      For A = 1 To 8                                        'wys³anie 8 bajtów kodu do porównania przez czujnik
         1wwrite Ar(a)                                      'Ÿród³o bajtów w tabeli kodu
      Next
   1wwrite &H44                                             'polecenie inicjalizacji konwersji
   Waitms 100                                               'pauza 100ms na czas konwersji z rozdzialczoœci¹ >9 bitów, wystarczy 8 bitów na czêœæ ca³kowit¹ temperatury

'odczyt temperatury i przetwarzanie wyniku do uzyskania tylko czêœci ca³kowitej
      1wreset
      1wwrite &H55
      For A = 1 To 8
         1wwrite Ar(a)
      Next
   1wwrite &HBE                                             'polecenie zwrócenia zawartoœci pamiêci czujnika

   A = 1wread()                                             'odczyt pierwszego bajtu pamiêci - TempL - mniej znaczacy bajt typu integer
   'przepisanie zawartoœci spod adresu A pod adres mniej znacz¹cego bajtu TempL
   LDS r24,{a}                                              'za³adowanie do rejestru procesora R24 spod adresu zmiennej A z SRAM
   STS {t},r24                                              'zachowanie zawartoœci R24 pod adresem zmiennej T w SRAM

   A = 1wread()                                             'odczyt bardziej znacz¹cego bajtu temperatury TempH - zawiera bit znaku temperatury
   LDS r24,{t}                                              'za³adowanie do rejestru procesora R24 spod adresu zmiennej T mniej znacz¹cego bajtu TempL
   LDS r25,{a}                                              'za³adowanie do rejestru procesora R25 spod adresu zmiennej A bardziej znacz¹cego bajtu TempH

   'podzielenie odczytanej z czujnika wartoœci Temp / 16 czyli 4x przesuniêcie 16 bitów w prawo
   'w r25 TempH; w r24 TempL
   ASR r25                                                  'przesuniêcie bitów w prawo tak, ¿e na pozycjê bitu 7 (znaku liczby) trafia bit jaki by³ przed przesuniêciem; bit wychodz¹cy trafia do C w SREG
   ROR r24                                                  'przesuniecie bitów w prawo tak, ¿e na pozycjê bitu 7 trafia bit C
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   ASR r25
   ROR r24
   STS {t},r24                                              'zachowanie mniej znacz¹cego bajtu temperatury TL pod adresem zmiennej T z offsetem +0 (bez offsetu)
   STS {t+1},r25                                            'zachowanie bardziej znacz¹cego bajtu temperatury TH pod adresem zmiennej T z offsetem +1
   RET                                                      'koñczy procedurê - powrót do miejsca wyjœcia -> za RCALL