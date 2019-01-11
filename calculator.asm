************************************************************************************
*
* Title: Calculator
*
* Objective: Perform basic arithmetic (+ - * /)
*
* Date: March 15, 2017
*
* Programmer: Robert Beck
*
* Company: The Pennsylvania State University
* Electrical Engineering and Computer Science
*
* Memory Use: RAM Locations from $3000 to $3009
*
* Input: simple expressions entered by user
* 
* Output: the calculated answers to the expressions
*
* Comments: This program This program is developed and simulated using CodeWarrior
* development software.
*
*************************************************************************************

;export symbols
              XDEF      Entry          ; export 'Entry' symbol
              ABSENTRY  Entry          ; for assembly entry point

;include derivative specific macros
PORTB         EQU     $0001
DDRB          EQU     $0003
;add more for the ones you need

SCISR1        EQU     $00cc            ; Serial port (SCI) Status Register 1
SCIDRL        EQU     $00cf            ; Serial port (SCI) Data Register

;following is for the TestTerm debugger simulation only
;SCISR1        EQU     $0203            ; Serial port (SCI) Status Register 1
;SCIDRL        EQU     $0204            ; Serial port (SCI) Data Register

CR            equ     $0d              ; carriage return, ASCII 'Return' key
LF            equ     $0a              ; line feed, ASCII 'next line' character

;variable/data section below
              ORG     $3000            ; RAMStart defined as $3000
                                       ; in MC9S12C128 chip ($3000 - $3FFF)

prompt        DC.B    'Ecalc> ', $00
cbuf          DS.B    10
num1          DS.B    2
num2          DS.B    2
ans           DS.B    2 
op            DS.B    1
num1buf       DS.B    3
num2buf       DS.B    3
num1ct        DS.B    1
num2ct        DS.B    1
;temp         DS.B    3
; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program.
                                  
StackSP                                ; Stack space reserved from here to
                                       ; StackST

              ORG  $3100
;code section below
Entry
              LDS   #Entry             ; initialize the stack pointer

              ldaa  #0
              staa  num1ct
              ldaa  #0
              staa  num2ct
              
mLoop         
              jsr   nextline
              ldx   #prompt
              jsr   printmsg
              ldx   #cbuf
loop2
              jsr   getchar
              cmpa  NULL
              beq   loop2
              
              staa  1,x+
              jsr   putchar
              cmpa  #CR
              bne   loop2
              jsr   separate
              bra   mLoop         
              
              
              
              
              
              

;subroutine section below

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************


;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************



;***********separate***************************

separate      psha
              
              ldx   #cbuf
              ldaa  1,x+
              cmpa  #$39
              bgt   invalid
              cmpa  #$30
              blt   invalid
              ldy   #num1buf
num1loop
              staa  1,y+
              inc   num1ct
              ldaa  1,x+
              cmpa  #$39
              bgt   optest
              cmpa  #$30
              blt   optest
              bra   num1loop
              
optest
              cmpa  #$2B
              beq   storeOp
              cmpa  #$2D
              beq   storeOp
              cmpa  #$2A
              beq   storeOp
              cmpa  #$2F
              beq   storeOp
              bra   invalid        
storeOp
              staa  op
              
              ldaa  1,x+
              cmpa  #$39
              bgt   invalid
              cmpa  #$30
              blt   invalid
              ldy   #num2buf
num2loop
              staa  1,y+
              inc   num2ct
              ldaa  1,x+
              cmpa  #$39
              bgt   rtest
              cmpa  #$30
              blt   rtest
              bra   num2loop
rtest
              cmpa  #CR
              bne   invalid
              jsr   calculate
              bra   endsep
invalid         
              jsr   nextline
              ldx   #msg4
              jsr   printmsg
endsep
              ldaa  #0
              staa  num1ct
              ldaa  #0
              staa  num2ct
              pula
              rts
              
;***********end of separate********************

;***********calculate***************************

calculate
              ldaa  num1ct
              cmpa  #1
              beq   oneonedigit
              cmpa  #2
              beq   onetwodigit
              cmpa  #3
              beq   onethreedigit
              lbra   invalid2
oneonedigit
              ldy   #num1buf
              ldaa  1,y+
              suba  #$30
              staa  num1
              bra   getnum2
onetwodigit
              ldy   #num1buf
              ldaa  1,y+
              suba  #$30
              ldab  #10
              mul
              stab  num1
              ldaa  1,y+
              suba  #$30 
              adda  num1
              staa  num1
              bra   getnum2
onethreedigit
              ldy   #num1buf
              ldaa  1,y+
              suba  #$30
              ldab  #100
              mul
              std   num1
              ldaa  1,y+
              suba  #$30
              ldab  #10
              addd  num1
              std   num1
              ldab  1,y+
              subb  #$30
              ldaa  #0
              addd  num1
              std   num1
getnum2
              ldaa  num2ct
              cmpa  #1
              beq   twoonedigit
              cmpa  #2
              beq   twotwodigit
              cmpa  #3
              beq   twothreedigit
              lbra   invalid2
twoonedigit
              ldy   #num2buf
              ldaa  1,y+
              suba  #$30
              staa  num2
              bra   compute
twotwodigit
              ldy   #num2buf
              ldaa  1,y+
              suba  #$30
              ldab  #10
              mul
              stab  num2
              ldaa  1,y+
              suba  #$30 
              adda  num2
              staa  num2
              bra   compute
twothreedigit
              ldy   #num2buf
              ldaa  1,y+
              suba  #$30
              ldab  #100
              mul
              std   num2
              ldaa  1,y+
              suba  #$30
              ldab  #10
              addd  num2
              std   num2
              ldab  1,y+
              subb  #$30
              ldaa  #0
              addd  num2
              std   num2
compute
              ldaa  op
              cmpa  #$2B
              beq   add
              cmpa  #$2D
              beq   sub
              cmpa  #$2A
              beq   multiply
              cmpa  #$2F
              beq   divide
add            
              ldd   num1
              addd  num2
              std   ans
              bra   calcdone
sub            
              ldd   num1
              subd  num2
              std   ans
              bra   calcdone
multiply
              ldd   num1
              ldy   num2
              emul
              ;cpy  #0
              ;bne  overflow
              std  ans
              bra  calcdone
divide
              ldd  num1
              ldx  num2
              idiv
              stx  ans
              bra  calcdone
overflow
              jsr  nextline
              ldx  #overr
              jsr  printmsg
              rts
invalid2
              jsr   nextline
              ldx   #msg4
              jsr   printmsg
calcdone
              jsr   printans
              rts
              
                    
;***********end of calculate********************

;***********printans***************************

printans
              ;ldaa  #2
              ;staa  ans
              jsr   nextline
              ldx   #cbuf
pLoop  
              ldaa  1,x+
              cmpa  #CR
              beq   cont
              jsr   putchar
              bra   pLoop
cont
              ldaa  #'='
              jsr   putchar
              
              ;ldd   ans
              ;cpd   #9999
             ; lbgt   fivedigit 
              ;cpd   #999
              ;bgt   fourdigit
              ;cpd   #99
              ;bgt   threedigit
              ;cpd   #9
              ;bgt   twodigit 
              
              ;tba
              ldaa  ans
              adda  #$30
              jsr   putchar
              lbra   endans  
twodigit
              ldx   #10
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              tba
              adda  #$30
              jsr   putchar
              lbra   endans
threedigit
              ldx   #100
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #10
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              tba
              adda  #$30
              jsr   putchar
              lbra   endans
fourdigit
              ldx   #1000
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #100
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #10
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              tba
              adda  #$30
              jsr   putchar
              bra   endans
              tba
              tba
              adda  #$30
              jsr   putchar
              bra   endans
fivedigit
              ldx   #10000
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #1000
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #100
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              ldx   #10
              idiv
              xgdx
              tba
              adda  #$30
              jsr   putchar
              xgdx
              tba
              adda  #$30
              jsr   putchar
              bra   endans
              tba
              tba
              adda  #$30
              jsr   putchar
endans
              rts


              
;***********end of printans********************

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************



;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 


;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

msg3           DC.B    'Enter your command below:', $00
msg4           DC.B    'Error: Invalid command', $00
overr          DC.B    'Error: Overflow', $00


               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled