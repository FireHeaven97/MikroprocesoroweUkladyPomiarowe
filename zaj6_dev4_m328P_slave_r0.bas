'Program urz¹dzenia slave DEV4 do konwersji w wewnêtrznym ADC w okresie 1s i zwracania danych na ¿¹danie master.
'Przerwania:
'URXC - odbioru bajtu przez USART0,
'UTXC - koñca nadawania przez USART0 - wy³¹cza nadajnik linii, w³¹cza odbiornik linii RS485,
'OC1A - do wyznaczenia okresów konwersji ADC.
'Timer1 w trybie PWM na kanale OC2B - wspó³czynnik wype³nienia impulsów DC = OCR2B/256.

'Ramka ¿adania odpowiedzi zawiera 2 znaki: _bof i &h20 (spacja), gdzie _bof to ma³a litera przypisana do stanowiska.
'_bof - znak z zakresu "a...r" na stanowisku o numerze z zakresu 1...18.
'_bofs - znak w ramce odpowiedzi to _bof zmieniony na wielk¹ literê.

'Format ramki odpowiedzi Dev4: _bods + ":" + 4  znaki HEX(w) + CR + LF.
'Ramka odpowiedzi jest nadawana po odbiorze ramki: _bofs + spacja

'by Marcin Kowalczyk
Const Prescfc = 0                       'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(16000000 /(2 ^ Prescfc))       'czêstotliwoœæ po przeskalowaniu
Const _u2x0 = 1                         'opcjonalne ustawienie bitu U2X0: 0-clr; 1-set
Const Baundrs = 115200                  'prêdkoœæ transmisji po RS [bps]
Const _ubrr =((((_u2x0 + 1) * Fcrystal / Baundrs) / 16) - 1)       'na potem
Const _fpr_adc = 1                      'czêstotliwoœæ probkowania ADC 1Hz
Const _presc_timer_adc = 1024           'dzielnik czêstoœci taktowania dla timer1
Const _ocr1a = Fcrystal / _presc_timer_adc / _fpr_adc -1       'wartoœæ rej. porównania do odmierzenia okresu próbkowania

'Indywidualne sta³e adresowe urz¹dzenia
Const _bofs = "A"                       'wielkie litery od &h41, ma³e litery od &h61; _bofs jako znak(byte)
Const _bof = Asc(_bofs) Or &H20         'przerobienie wielkiej litery na ma³¹; _bof jako byte

Const _time2resp_us = 3                 'opóŸnienie nadawania od w³¹czenia nadajnika linii [us]
Const _wait2resp =(_time2resp_us * Fcrystal) \ 3 \ 1000000 + 1       'musi byæ <=255

$regfile = "m328pdef.dat"               'nazwa pliku konfiguracyjnego mikrokontrolera
$crystal = Fcrystal                     'czêœtotliwoœæ taktowania procesora
'$baud = Baundrs                        'konfiguracja RS w procedurze usart_init
$programmer = 3                         'wywo³anie programatora z arduino bootloader

'aliasy rejestrów procesora
Temp Alias R24                          'alias rejestru procesora R24 - w kodzie poni¿ej wpisu mo¿na u¿yæ nazwy temp zamiast R24
Temph Alias R25
Rstemp Alias R23
Rsdata Alias R22
'pozosta³e aliasy
Te_pin Alias 4                          'nr bitu (wyprowadzenia) portu D do sterowania nadajnikiem linii RS485

'Ustawiania
'ustawienie preskalera czêstoœci taktowania mikrokontrolera
Clkpr = &H80                            '&B10000000 , aktywacja aktualizacji CLKPR
Clkpr = Prescfc                         'wpisanie nowej wartoœci CLKPR

rcall usart_init                        'inicjalizacja USART

'konfiguracja timer2 w trybie PWM z u¿yciem wyprowadzenia OC2B (PD.6)
'Uwaga:od wersji bascom 2075 zamiast Clear Down u¿yæ Clear Up
Config Timer2 = Pwm , Prescale = 8 , Compare B Pwm = Clear Down
Ocr2b = 128                             'DC = 0,5 = 128/256

'konfiguracja timer1
Config Timer1 = Timer , Prescale = _presc_timer_adc , Compare A = Disconnect , Clear Timer = 1
On Oc1a Przerwanie_oc1a Nosave          'deklaracja przerwania - zainicjowanie konwersji temperatury
Stop Timer1                             'zatrzymanie timer1
Timer1 = 0                              'wyzerowanie licznika
Ocr1a = _ocr1a                          'wartoœæ rej. A porównania - przerwanie OC1A wyzeruje licznik timer1

Config Adc = Single , Prescaler = Auto , Reference = Avcc       'Ure f= AVCC = VCC = 5V
Start Adc                               'w³¹czenie zasilania (bit 6)

'deklaracje zmiennych w SRAM: A-pomocniczy bajt do pêtli i operacji odczytu 1wire; T - temperatura [st.C]
Dim A As Byte , W As Integer            'deklaracja zmiennej byte w SRAM
Dim Konv_status As Byte                 'bajt statusu konwersji: 0- okres oczekiwania na inicjalizacjê konwersji;
Dim Udra As Byte                        'poprzednio odebrany byte
'1- stan inicjalizacji konwersji; 2- stan odczytu temperatury i dalszego przetwarzania
Dim Strout As String * 32               'string max. 32 znaki

On Utxc Tx0end Nosave                   'definicja procedury przerwania koñca nadawania USART0
On Urxc Rx0 Nosave                      'definicja procedury przerwania po odbiorze znaku w USART0

'W³¹czenie przerwañ i timer1
Start Timer1                            'wystartowanie timer1
Enable Oc1a                             'w³¹czenie przerwania porównania A
Enable Utxc                             'w³¹czenie przerwania koñca nadawania
Enable Urxc                             'w³¹czenie przerwania odbioru znaku
Sei                                     'Enable Interrupts

'Pêtla g³ówna
Do                                      'pocz¹tek petli Do - Loop

   'Warunkowa Inicjalizacja Konwersji I Utworzenie Stringu Ramki Danych - Wersja Rozwojowa Do Poprawy
   If Konv_status = 1 Then              'gdy w³¹czono stan konwersji
      W = Getadc(0)                     'zainicjowanie konwersji ADC, konwersja i przepisanie wyniku do zmiennej W
      Strout = _bofs + ":" + Hex(w) + Chr(13) + Chr(10)       'ramka odpowiedzi z konwersj¹ W na tekst HEX
      Konv_status = 0                   'oznaczenie stanu oczekowania na zezwolenie na konwersjê
   End If

Loop


Przerwanie_oc1a:                        'procedura przerwania DO POPRAWY
'U¿yte operacje nie zmieniaj¹ SREG, ale u¿ywaj¹ R24
   push temp                            'zachowanie na stosie wartoœci rej. R24                          <- 2CTP
   ldi temp,1                           'za³adowanie wartoœci 1                                          <- 1CTP
   sts {Konv_status},temp               'zachowanie wartoœic R24 pod adresem zmiennej Konv_status w SRAM <- 2CTP
   pop temp                             'odtworzenie R24                                                 <- 2CTP
Return                                  'RETI                                                     <- 4CTP


Rx0:                                    'procedura przerwania odbioru danych z USART0 - wersja rozwojowa
   'W u¿yciu: rejestry procesora R22; R23 oraz SREG
   push rstemp
   in rstemp,sreg
   push rstemp
   push rsdata

   in rsdata,udr0                       'przepisanie odebranego znaku do R23
      lds rstemp,{udra}                 'za³adowanie do R22 poprzednio odebranego bajtu - nie zmiania SREG
      sts {udra},rsdata                 'zachowanie odebranego bajtu pod adresem zmiennej
   'czy odebrano spacjê
   cpi rsdata,&h20                      'czy odebrano spacjê - ustala bity C i Z w SREG
   'czy odebrano BOF i spacjê
   ldi rsdata,_bof                      'za³adowanie do R22 adresu w³asnego - nie zmiania SREG
   cpc rstemp,rsdata                    'porównanie R23 z R22 z uwzglêdnienem bitu C poprzedniego porównania
   brne rx0_end
      'zainocjowanie nadawania odpowiedzi - wersja rozwojowa
      sbi portd,Te_pin                  'w³¹czenie nadajnika linii
      rcall wait2transmit               'opóŸnienie nadawania do zapewnienia w³¹czenia nadajnika linii

      'Pierwsze znaki ramki odpowiedzi
      ldi rsdata,asc(_bofs)
      !out udr0,rsdata                  'wpisanie bajtu ropoczynaj¹cego transmisjê
      ldi rsdata,asc(":")
      'transmisja rozpoczê³a siê, a UDR0 jest pusty
      !out udr0,rsdata
         'Wpisywanie kolejnych znaków wymaga kontroli bitu UDRE0
         'Czas transmisji jednego znaku ok. 86,8us (115200bps), a 6 znaków 521us
         lds rsdata,{strout+2}          'znak stringu z offsetem 2 (3. znak stringu)
         rcall _wait4udre
         lds rsdata,{strout+3}          'znak stringu z offsetem 3 (4. znak stringu)
         rcall _wait4udre
         lds rsdata,{strout+4}          'znak stringu z offsetem 4 (5. znak stringu)
         rcall _wait4udre
         lds rsdata,{strout+5}          'znak stringu z offsetem 5 (6. znak stringu)
         rcall _wait4udre
         lds rsdata,{strout+6}          'znak stringu z offsetem 6 (7. znak stringu)
         rcall _wait4udre
         lds rsdata,{strout+7}          'znak stringu z offsetem 7 (8. znak stringu)
         rcall _wait4udre
         'Wyjœcie krótko po rozpoczêciu transmisji przedostatniego znaku.
         'Ostatni znak czeka w rejestrze UDR0
   !rx0_end:
   'odtworzenie stanu
   pop rsdata
   pop rstemp
   !out sreg,rstemp                     'zapis bajtu w rejestrze 1 CTP
   pop rstemp                           'zdjêcie danej ze stosu, 2 CTP
Return                                  'RETI

!_wait4udre:                            'procedura nadawania znaku
   sbis ucsr0a,udre0                    'obejœcie RJMP gdy UDRE0 set
      rjmp _wait4udre
   !out udr0,rsdata
ret

!wait2transmit:                         'procedura opóŸniaj¹ca
   'Waitus _time2resp_us' waitus u¿ywa ZL i ZH
   'U¿ywa rejestr procesora rstemp w procedurze przerwania - mo¿e byæ niezbêdne odtworzenie stanu rstemp.
   push rstemp                          '2 CTP
   ldi rstemp,_wait2resp                'liczba cykli taktowania procesora (_wait2resp * 3) CTP
   !wait_tx:                            'wykonanie pêtli (1+2) CTP
      dec rstemp                        '1 CTP
   brne wait_tx                         '2 CTP
   pop rstemp                           '2 CTP
ret


Tx0end:                                 'procedura przerwania koñca nadawania USART0
   cbi portd,Te_pin                     'wy³¹czenie nadajnika, w³¹czenie odbiornika linii
Return                                  'RETI

!usart_init:
'procedura inicjalizacji USART(ów)
   ldi temp,0
   !out ubrr0h,temp                     'bardziej znacz¹cy bajt UBRR USART0
   '!out ubrr1h,temp
   ldi temp,_ubrr
   !out ubrr0l,temp                     'mniej znacz¹cy bajt UBRR USART0
   '!out ubrr1l,temp                     'mniej znacz¹cy bajt UBRR USART1
   ldi temp,24                          'w³¹czone odbiorniki i nadajniki USARTów
   !out ucsr0b,temp
   '!out ucsr1b,temp
   ldi temp,6                           'N8bit
   !out ucsr0C,temp
   '!out ucsr1C,temp
   #if _u2x0 = 1                        'warunkowa kompilacja kodu
      sbi ucsr0a,u2x0                   'podwojenie prêdkoœci transmisji wzglêdem ustawienia w UBRR0
      'sbi ucsr1a,u2x1                   'podwojenie prêdkoœci transmisji wzglêdem ustawienia w UBRR1
   #endif

   'ustawienia RS485 - domyœlnie stan odbioru
   cbi portd,te_pin                     'nadajnik wy³¹czony
   sbi ddrd,Te_pin                      'wyjœcie TE silnopr¹dowe
   'w³¹czenie przerwañ - opcjonalnie tu
   'Enable Urxc
   'Enable Urxc1
   'Enable Utxc1
ret