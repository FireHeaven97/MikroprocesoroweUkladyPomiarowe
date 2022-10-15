'Program SPY
'Szymon Palmowski 235911

'sta³e, uwaga: u¿ycie nawiasow wymusza kolejnoœæ dzia³ania operacji równorzêdnych
Const Prescfc = 1                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
Const _u2x0 = 1                                             'opcjonalne ustawienie bitu U2X0: 0-clr; 1-set
Const Baundrs = 115200                                      'prêdkoœæ transmisji po RS [bps]
Const _ubrr =((((_u2x0 + 1) * Fcrystal / Baundrs) / 16) - 1)       'na potem

'konfiguracja mikrokontrolera ATmega644PA
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny mikrokontrolera, zawiera wszystkie aliasy rejestrów i bitów
$crystal = Fcrystal
$programmer = 13                                            'wywo³anie programatora z MCS bootloader

'alias rejestrów procesora
Temph Alias R25
Temp Alias R24                                              'alias rejestru procesora R24 - w kodzie poni¿ej wpisu mo¿na u¿yæ nazwy temp zamiast R24
Rsdata Alias R22

'pozosta³e aliasy
Te_pin Alias 4                                              'nr bitu (wyprowadzenia) portu D do sterowania nadajnikiem linii RS485

'ustawienie preskalera czêstoœci taktowania mikrokontrolera
Clkpr = &H80                                                '&B10000000 , aktywacja aktualizacji CLKPR
Clkpr = Prescfc                                             'wpisanie nowej wartoœci CLKPR

rcall usart_init                                            'inicjalizacja USARTów

Const Rb = 10
Const Rbuff = 2 ^ Rb                                        'rozmiar bufora

'deklaracje zmiennych w SRAM
Dim Udra As Byte                                            'poprzednio odebrany bajt
Dim Strw As String * 3                                      'poœredni string wartoœci HEX œrednej ruchomej
Dim Buff(rbuff) As Byte                                     'buffor wejœciowy, rozmiar w Bascom liczony od 1
Dim Aw As Word                                              'offset zapisu w buforze
Dim Ar As Word                                              'offset odczytu w buforze

On Urxc1 Odbior_rx Nosave                                   'definicja procedury przerwania po odbiorze znaku w USART1

'W³¹czenie przerwañ
Enable Urxc1                                                'w³¹czenie przerwania odbioru znaku
Sei                                                         'Enable Interrupts

Do                                                          'pêtla g³ówna
   lds temp,{ar}                                            'za³adowanie AR_L - mniej znacz¹cego bajtu AR
   lds temph,{ar+1}                                         'za³adowanie AR_H - bardziej znacz¹cego bajtu AR
   !nadaj_znak:
      lds xl,{aw}                                           'za³adowanie AW_L - mniej znacz¹cego bajtu AW
      lds xh,{aw+1}                                         'za³adowanie AW_H - bardziej znacz¹cego bajtu AW
      cp temp,xl
      cpc temph,xh
      breq send_buff_end                                    'opuszczenie pêtli nadaj_znak gdy AR=AW
         Loadadr Buff(1) , X                                'pobranie bajtu z buffora i nadanie w XL i XH adres pierwszego bajtu tabeli Buff, dodanie bajtu AR do s³owa X
         add xl,temp                                        'dodawanie najmniej znacz¹cych bajtów, XL + AR_L
         adc xh,temph                                       'dodawanie bardziej znacz¹cych bajtów XH + AR_H + CADC xh,temp                                        'dodawanie bardziej znacz¹cych bajtów XH + 0 + C
            LD temp,x                                       'za³adowanie bajtu z SRAM i inkrementacja adresu
               STS {udra},temp
               Strw = Hex(udra)                             'konwersja na HEX
               lds rsdata,{strw}                            'pierwszy znak HEX
               rcall send
               lds rsdata,{strw+1}                          'drugi znak HEX
               rcall send
               ldi rsdata,32                                'spacja w kodzie ACSI wpisana do rsdata
               rcall send
               lds rsdata, {udra}
               sbrc rsdata,7                                'jeœli 7 bit jest 0 to znaczy ¿e to dane
                  RCALL sprawdz_eof
         lds temp,{ar}                                      'za³adowanie AR_L - mniej znacz¹cego bajtu AR
         lds temph,{ar+1}                                   'za³adowanie AR_H - bardziej znacz¹cego bajtu AR
         subi temp,&hff
         sbci temph,&hff
         andi temp,(rbuff-1)
         andi temph,(rbuff\256-1)
         sts {ar},temp
         sts {ar+1},temph
   rjmp nadaj_znak                                          'przejœcie na pocz¹tek pêtli
   !send_buff_end:
Loop

Odbior_rx:
'procedura odbioru w przerwaniu
'Zachowanie na strosie rejestrów u¿ywanych w procedurze przerwania - przy³ad zachowania wszystkich rejestrów u¿ytych w przerwaniu.
'Trzeba zachowaæ i póŸniej odtworzyæ stan rtejestraów wspó³dzielonych w petli g³ównej, a nie koniecznie wszystkiego.
   push temp
   in temp,sreg
   push temp
   push temph
   push xl
   push xh

   rcall Rx1                                                'wyjœcie do procedury odbioru zdefiniowanej w Rx1

'odtworzenie stanu rejestrów - w odwrotnej kolejnoœci zdejmowanie ze stosu
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
   lds temph,{Aw}                                           'za³adowanie offsetu zapisu
   Loadadr Buff(1) , X                                      'za³adowanie do pary adresowej X: XL(r26) i XH(r27) adresu pierwszego bajtu buffora
   ADD xl,temph                                             'dodanie bajtu offsetu do s³owa adresu pocz¹tkowego, wynik przepe³nienia w C
   lds temph,{aw+1}                                         'za³adowanie bardziej znacz¹cego bajtu offsetu zapisu - nie powoduje zmian w SREG'utrata informacji o offsecie
   adc xh,temph                                             'do bardziej znacz¹cego bajtu adresu dodanie 0+C
      st x+,temp                                            'wpisanie odebranego bajtu do SRAM i inkrementaja adresu w parze adresowej X
   LDS temp,{Aw}                                            'za³adowanie offsetu zapisu
   subi temp,(256-1)                                        'dodanie 2 do s³owa poprzez odejmowanie -2 (FFh;254) od s³owa
   sbci temph,&hff                                          'odjêcie bardziej znacz¹cego bajtu liczby -2 od bardziej znacz¹cego bajtu offsetu AW
   andi temp,(rbuff-1)
   andi temph,(rbuff\256-1)                                 'gdy RBuff>=512 to rbuff\256 >=1; gdy RBuff=256 to rbuff\256-1=0;
   sts {aw},temp
   sts {aw+1},temph
   lds xl,{ar}                                              'za³adowanie offsetu odczytu
   lds xh,{ar+1}                                            'za³adowanie offsetu odczytu
   cp temp,xl                                               'porównanie rejestrów mniej znacz¹cych
   cpc temph,xh                                             'porównanie rejestrów bardziej znacz¹cych z C
   brne Rx1_end                                             'wyjœcie gdy nie ma nadpisywania danych w bufforze
      subi xl,(256-1)                                       'dodanie 2 do s³owa poprzez odejmowanie -2 (FFh;254) od s³owa
      sbci xh,&hff                                          'odjêcie bardziej znacz¹cego bajtu liczby -2 od bardziej znacz¹cego bajtu offsetu AR
      andi xl,(rbuff-1)
      andi xh,(rbuff\256-1)                                 'gdy RBuff>=512 to rbuff\256 >=1; gdy RBuff=256 to rbuff\256-1=0; znak \ oznacza dzielenie bez zaokr¹glania do najbli¿szej w.
      sts {ar},xl
      sts {ar+1},xh
!Rx1_end:
Ret                                                         'powrót tu¿ po miejscu wyjœcia do procedury RCALL

!sprawdz_eof:
sbrc rsdata,6                                               'jeœli 7 bit = 1, a 6 = 1 to znaczy ¿e eof, trzeba nadac CR LF
   RCALL koniec_linii
RET

!koniec_linii:
   LDI rsdata, 13                                           'wys³anie znaku 13 - powrót karetki
   RCALL send
   LDI rsdata, 10                                           'wys³anie znaku 10 - nowa linia
   RCALL send
Ret

!send:                                                      'w temp bajt do wyslania
   sbis ucsr0a,udre0                                        'obejœcie RJMP gdy UDR0 pusty
      rjmp send
   !out udr0,rsdata                                         'wpisanie do bufora nadajnika USART0
ret

!usart_init:
'procedura inicjalizacji USART(ów)
   ldi temp,0
   !out ubrr0h,temp                                         'bardziej znacz¹cy bajt UBRR USART0
   !out ubrr1h,temp
   ldi temp,_ubrr
   !out ubrr0l,temp                                         'mniej znacz¹cy bajt UBRR USART0
   !out ubrr1l,temp                                         'mniej znacz¹cy bajt UBRR USART1
   ldi temp,24                                              'w³¹czone odbiorniki i nadajniki USARTów
   !out ucsr0b,temp
   !out ucsr1b,temp
   ldi temp,6                                               'N8bit
   !out ucsr0C,temp
   !out ucsr1C,temp
   #if _u2x0 = 1                                            'warunkowa kompilacja kodu
      sbi ucsr0a,u2x0                                       'podwojenie prêdkoœci transmisji wzglêdem ustawienia w UBRR0
      sbi ucsr1a,u2x1                                       'podwojenie prêdkoœci transmisji wzglêdem ustawienia w UBRR1
   #endif

   'ustawienia RS485 - domyœlnie stan odbioru
   cbi portd,te_pin                                         'zerowanie bitu - nadajnik wy³¹czony
   sbi ddrd,Te_pin                                          'ustawienie bitu - wyjœcie TE silnopr¹dowe
ret
Return