'Program SPY
'Szymon Palmowski 235911

'sta�e, uwaga: u�ycie nawiasow wymusza kolejno�� dzia�ania operacji r�wnorz�dnych
Const Prescfc = 1                                           'pot�ga dzielnika cz�stotliwo�ci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'cz�stotliwo�� po przeskalowaniu
Const _u2x0 = 1                                             'opcjonalne ustawienie bitu U2X0: 0-clr; 1-set
Const Baundrs = 115200                                      'pr�dko�� transmisji po RS [bps]
Const _ubrr =((((_u2x0 + 1) * Fcrystal / Baundrs) / 16) - 1)       'na potem

'konfiguracja mikrokontrolera ATmega644PA
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny mikrokontrolera, zawiera wszystkie aliasy rejestr�w i bit�w
$crystal = Fcrystal
$programmer = 13                                            'wywo�anie programatora z MCS bootloader

'alias rejestr�w procesora
Temph Alias R25
Temp Alias R24                                              'alias rejestru procesora R24 - w kodzie poni�ej wpisu mo�na u�y� nazwy temp zamiast R24
Rsdata Alias R22

'pozosta�e aliasy
Te_pin Alias 4                                              'nr bitu (wyprowadzenia) portu D do sterowania nadajnikiem linii RS485

'ustawienie preskalera cz�sto�ci taktowania mikrokontrolera
Clkpr = &H80                                                '&B10000000 , aktywacja aktualizacji CLKPR
Clkpr = Prescfc                                             'wpisanie nowej warto�ci CLKPR

rcall usart_init                                            'inicjalizacja USART�w

Const Rb = 10
Const Rbuff = 2 ^ Rb                                        'rozmiar bufora

'deklaracje zmiennych w SRAM
Dim Udra As Byte                                            'poprzednio odebrany bajt
Dim Strw As String * 3                                      'po�redni string warto�ci HEX �rednej ruchomej
Dim Buff(rbuff) As Byte                                     'buffor wej�ciowy, rozmiar w Bascom liczony od 1
Dim Aw As Word                                              'offset zapisu w buforze
Dim Ar As Word                                              'offset odczytu w buforze

On Urxc1 Odbior_rx Nosave                                   'definicja procedury przerwania po odbiorze znaku w USART1

'W��czenie przerwa�
Enable Urxc1                                                'w��czenie przerwania odbioru znaku
Sei                                                         'Enable Interrupts

Do                                                          'p�tla g��wna
   lds temp,{ar}                                            'za�adowanie AR_L - mniej znacz�cego bajtu AR
   lds temph,{ar+1}                                         'za�adowanie AR_H - bardziej znacz�cego bajtu AR
   !nadaj_znak:
      lds xl,{aw}                                           'za�adowanie AW_L - mniej znacz�cego bajtu AW
      lds xh,{aw+1}                                         'za�adowanie AW_H - bardziej znacz�cego bajtu AW
      cp temp,xl
      cpc temph,xh
      breq send_buff_end                                    'opuszczenie p�tli nadaj_znak gdy AR=AW
         Loadadr Buff(1) , X                                'pobranie bajtu z buffora i nadanie w XL i XH adres pierwszego bajtu tabeli Buff, dodanie bajtu AR do s�owa X
         add xl,temp                                        'dodawanie najmniej znacz�cych bajt�w, XL + AR_L
         adc xh,temph                                       'dodawanie bardziej znacz�cych bajt�w XH + AR_H + CADC xh,temp                                        'dodawanie bardziej znacz�cych bajt�w XH + 0 + C
            LD temp,x                                       'za�adowanie bajtu z SRAM i inkrementacja adresu
               STS {udra},temp
               Strw = Hex(udra)                             'konwersja na HEX
               lds rsdata,{strw}                            'pierwszy znak HEX
               rcall send
               lds rsdata,{strw+1}                          'drugi znak HEX
               rcall send
               ldi rsdata,32                                'spacja w kodzie ACSI wpisana do rsdata
               rcall send
               lds rsdata, {udra}
               sbrc rsdata,7                                'je�li 7 bit jest 0 to znaczy �e to dane
                  RCALL sprawdz_eof
         lds temp,{ar}                                      'za�adowanie AR_L - mniej znacz�cego bajtu AR
         lds temph,{ar+1}                                   'za�adowanie AR_H - bardziej znacz�cego bajtu AR
         subi temp,&hff
         sbci temph,&hff
         andi temp,(rbuff-1)
         andi temph,(rbuff\256-1)
         sts {ar},temp
         sts {ar+1},temph
   rjmp nadaj_znak                                          'przej�cie na pocz�tek p�tli
   !send_buff_end:
Loop

Odbior_rx:
'procedura odbioru w przerwaniu
'Zachowanie na strosie rejestr�w u�ywanych w procedurze przerwania - przy�ad zachowania wszystkich rejestr�w u�ytych w przerwaniu.
'Trzeba zachowa� i p�niej odtworzy� stan rtejestra�w wsp�dzielonych w petli g��wnej, a nie koniecznie wszystkiego.
   push temp
   in temp,sreg
   push temp
   push temph
   push xl
   push xh

   rcall Rx1                                                'wyj�cie do procedury odbioru zdefiniowanej w Rx1

'odtworzenie stanu rejestr�w - w odwrotnej kolejno�ci zdejmowanie ze stosu
   pop xh
   pop xl
   pop temph
   pop temp
   !out sreg,temp
   pop temp
Return

'procedura przerwania odbioru danych z USART1
!Rx1:                                                       'etykieta ASM procedury i adres w flash
   In temp,udr1                                             'przepisanie odebranego bajtu do rejestru procesora powoduje wyzerowanie bitu UCSR0A.RXC0
   lds temph,{Aw}                                           'za�adowanie offsetu zapisu
   Loadadr Buff(1) , X                                      'za�adowanie do pary adresowej X: XL(r26) i XH(r27) adresu pierwszego bajtu buffora
   ADD xl,temph                                             'dodanie bajtu offsetu do s�owa adresu pocz�tkowego, wynik przepe�nienia w C
   lds temph,{aw+1}                                         'za�adowanie bardziej znacz�cego bajtu offsetu zapisu - nie powoduje zmian w SREG'utrata informacji o offsecie
   adc xh,temph                                             'do bardziej znacz�cego bajtu adresu dodanie 0+C
      st x+,temp                                            'wpisanie odebranego bajtu do SRAM i inkrementaja adresu w parze adresowej X
   LDS temp,{Aw}                                            'za�adowanie offsetu zapisu
   subi temp,(256-1)                                        'dodanie 2 do s�owa poprzez odejmowanie -2 (FFh;254) od s�owa
   sbci temph,&hff                                          'odj�cie bardziej znacz�cego bajtu liczby -2 od bardziej znacz�cego bajtu offsetu AW
   andi temp,(rbuff-1)
   andi temph,(rbuff\256-1)                                 'gdy RBuff>=512 to rbuff\256 >=1; gdy RBuff=256 to rbuff\256-1=0;
   sts {aw},temp
   sts {aw+1},temph
   lds xl,{ar}                                              'za�adowanie offsetu odczytu
   lds xh,{ar+1}                                            'za�adowanie offsetu odczytu
   cp temp,xl                                               'por�wnanie rejestr�w mniej znacz�cych
   cpc temph,xh                                             'por�wnanie rejestr�w bardziej znacz�cych z C
   brne Rx1_end                                             'wyj�cie gdy nie ma nadpisywania danych w bufforze
      subi xl,(256-1)                                       'dodanie 2 do s�owa poprzez odejmowanie -2 (FFh;254) od s�owa
      sbci xh,&hff                                          'odj�cie bardziej znacz�cego bajtu liczby -2 od bardziej znacz�cego bajtu offsetu AR
      andi xl,(rbuff-1)
      andi xh,(rbuff\256-1)                                 'gdy RBuff>=512 to rbuff\256 >=1; gdy RBuff=256 to rbuff\256-1=0; znak \ oznacza dzielenie bez zaokr�glania do najbli�szej w.
      sts {ar},xl
      sts {ar+1},xh
!Rx1_end:
Ret                                                         'powr�t tu� po miejscu wyj�cia do procedury RCALL

!sprawdz_eof:
sbrc rsdata,6                                               'je�li 7 bit = 1, a 6 = 1 to znaczy �e eof, trzeba nadac CR LF
   RCALL koniec_linii
RET

!koniec_linii:
   LDI rsdata, 13                                           'wys�anie znaku 13 - powr�t karetki
   RCALL send
   LDI rsdata, 10                                           'wys�anie znaku 10 - nowa linia
   RCALL send
Ret

!send:                                                      'w temp bajt do wyslania
   sbis ucsr0a,udre0                                        'obej�cie RJMP gdy UDR0 pusty
      rjmp send
   !out udr0,rsdata                                         'wpisanie do bufora nadajnika USART0
ret

!usart_init:
'procedura inicjalizacji USART(�w)
   ldi temp,0
   !out ubrr0h,temp                                         'bardziej znacz�cy bajt UBRR USART0
   !out ubrr1h,temp
   ldi temp,_ubrr
   !out ubrr0l,temp                                         'mniej znacz�cy bajt UBRR USART0
   !out ubrr1l,temp                                         'mniej znacz�cy bajt UBRR USART1
   ldi temp,24                                              'w��czone odbiorniki i nadajniki USART�w
   !out ucsr0b,temp
   !out ucsr1b,temp
   ldi temp,6                                               'N8bit
   !out ucsr0C,temp
   !out ucsr1C,temp
   #if _u2x0 = 1                                            'warunkowa kompilacja kodu
      sbi ucsr0a,u2x0                                       'podwojenie pr�dko�ci transmisji wzgl�dem ustawienia w UBRR0
      sbi ucsr1a,u2x1                                       'podwojenie pr�dko�ci transmisji wzgl�dem ustawienia w UBRR1
   #endif

   'ustawienia RS485 - domy�lnie stan odbioru
   cbi portd,te_pin                                         'zerowanie bitu - nadajnik wy��czony
   sbi ddrd,Te_pin                                          'ustawienie bitu - wyj�cie TE silnopr�dowe
ret
Return