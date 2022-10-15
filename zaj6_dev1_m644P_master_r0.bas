'Program urz�dzenia master do zadania 6 TN na DEV1 ATmega644PA.
'Przerwania:
'URXC1 - odbioru bajtu z USART1
'UTXC1 - ko�ca nadawania USART1 do wy��czenia nadajnika linii

'Ramka zapytania (master) zawiera dwa znaki: BOF i spacji.
'Znak BOF zale�y od numeru stanowiska. Stanowisku 1 odpowiada znak "a", stanowisku 2 znak "b", itd..

'Ramka odpowiedzi (slave) zawiera: znak BOFs - podobny jak przydzielony BOF, ale jest wielk� liter�;
'znak ":"; 4 znaki �redniej warto�ci po konwersji typu integer w zapisie szesnastkowym; znaki CR i LF.

'W p�tli g�wnej w okresie ok. 1s wysy�ane jest 18 ramek ��dania odpowiedzi przez RS485.
'Po ka�dej ramce mastera jest pauza na odpowied� d�u�sza o czasu nadawania 8 znak�w o czas transmisji 1 znaku.
'W czasie Time_resp_us uwzgl�dniono czas transmisji 2 znak�w mastera - wpisanie do UDR1 tylo inicjuje transmisj�.

'by Marcin Kowalczyk
Const Prescfc = 1                       'pot�ga dzielnika cz�stotliwo�ci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))       'cz�stotliwo�� po przeskalowaniu
Const _u2x0 = 1                         'opcjonalne ustawienie bitu U2X0: 0-clr; 1-set
Const Baudrs = 115200                   '/ 2 ^ _u2x0      'pr�dko�� transmisji po RS [bps]
Const _ubrr =((((_u2x0 + 1) * Fcrystal / Baudrs) / 16) - 1)       'na potem

Const Lenframes = 2 + 4 + 2             'BOFs + ":" + HEX(int) + CR + LF
Const Time_resp_us = 1000000 * 10 *(lenframes + 2 + 1) / Baudrs       'czas na odpowiedz uwzgl�dnia czas nadawania ramki zapytania + zapas

$regfile = "m644pdef.dat"               'nazwa pliku konfiguracyjnego mikrokontrolera
$crystal = Fcrystal                     'cz�totliwo�� taktowania procesora
$programmer = 13                        'wywo�anie programatora z MCS bootloader

'aliasy rejestr�w procesora
Temp Alias R24                          'alias rejestru procesora R24 - w kodzie poni�ej wpisu mo�na u�y� nazwy temp zamiast R24
Temph Alias R25

'pozosta�e aliasy
Te_pin Alias 4                          'nr bitu (wyprowadzenia) portu D do sterowania nadajnikiem linii RS485

'Ustawiania
'ustawienie preskalera cz�sto�ci taktowania mikrokontrolera
Clkpr = &H80                            '&B10000000 , aktywacja aktualizacji CLKPR
Clkpr = Prescfc                         'wpisanie nowej warto�ci CLKPR

rcall usart_init                        'inicjalizacja USART�w

'deklaracje zmiennych w SRAM:
Dim A As Byte
Dim Addrs As Byte                       'adres urz�dzenia

On Urxc1 Usart1_rx Nosave               'definicja przerwania URXC1 - odbioru znaku przez USART1
On Utxc1 Usart1_tx Nosave               'definicja przerwania UTXC1  - ko�ca nadawania

Enable Urxc1                            'w��czenie przerwania URXC1
Enable Utxc1                            'w��czenie przerwania UTXC1 - tego brakowa�o na zaj�ciach 5 TP
Sei                                     'Enable Interrupts

Addrs = "a"                             'bajt adresu urz�dzenia przy stanowisku 1

'P�tla g��wna
Do                                      'pocz�tek petli Do - Loop
   sbi portd,Te_pin                     'w��czenie nadajnika linii
   'Waitus 3                             'gdy s� przek�amania znak�w - niekt�re nadajniki linii maj� d�u�szy czas w��czania si�
   lds temp,{Addrs}                     'za�adowanie znaku adresu ze zmiennej w SRAM
   !out udr1,temp                       'zainocjowanie transmisji ma magistrale RS485
   !out udr0,temp                       'zainocjowanie transmisji do komputera
      ldi temp,&h20                     'za�adowanie znaku spacji warto�ci sta�ej
      !out udr1,temp                    'wpisanie do UDR1 znaku do transmisji przez RS485
      !out udr0,temp                    'wpisanie do UDR1 znaku do transmisji do komputera

   Waitus Time_resp_us                  'pauza czasu na nadawanie dw�ch znak�w zapytania i odbi�r ramki odpowiedzi

   Incr Addrs
   If Addrs > "r" Then
      Addrs = "a"                       'zakres adres�w "a...r"
      Print
      Waitms 983                        'przybli�one dope�nienie pauzy do ok. 1s
   End If

   'obs�uga wej�cia danych z komputera
   If Ucsr0a.rxc0 = 1 Then              'gdy bit RXC0 ustawiony - odebrany bajt w UDR0
      A = Udr0                          'przepisanie bajtu do zmiennej A, wyczyszczenie RXC0
      If A = "P" Then Start Watchdog    'gdy odebran znak "P" wystartowanie watchdoga i restart uC
   End If
Loop


Usart1_tx:
   cbi portd,Te_pin                     'wy��czenie nadajnika linii RS485
Return

Usart1_rx:                              'procedura odbioru w USART1 w przerwaniu URXC1
   push temp                            'u�ywany R24
   'odbi�r znaku z magistrali RS485 rozpoczyna jego transmisj� do komputera
   in temp,udr1
   !out udr0,temp
   pop temp                             'odtworzenie stanu
Return


!usart_init:
'procedura inicjalizacji USART(�w)
   ldi temp,0
   !out ubrr0h,temp                     'bardziej znacz�cy bajt UBRR USART0
   !out ubrr1h,temp
   ldi temp,_ubrr
   !out ubrr0l,temp                     'mniej znacz�cy bajt UBRR USART0
   !out ubrr1l,temp                     'mniej znacz�cy bajt UBRR USART1
   ldi temp,24                          'w��czone odbiorniki i nadajniki USART�w
   !out ucsr0b,temp
   !out ucsr1b,temp
   ldi temp,6                           'N8bit
   !out ucsr0C,temp
   !out ucsr1C,temp
   #if _u2x0 = 1                        'warunkowa kompilacja kodu
      sbi ucsr0a,u2x0                   'podwojenie pr�dko�ci transmisji wzgl�dem ustawienia w UBRR0
      sbi ucsr1a,u2x1                   'podwojenie pr�dko�ci transmisji wzgl�dem ustawienia w UBRR1
   #endif

   'ustawienia RS485 - domy�lnie stan odbioru
   cbi portd,te_pin                     'nadajnik wy��czony
   sbi ddrd,Te_pin                      'wyj�cie TE silnopr�dowe
   'w��czenie przerwa� - opcjonalnie tu
   'Enable Urxc
   'Enable Urxc1
   'Enable Utxc1
ret