;*******************************************************
;* CMPEN 472, HW8 Real Time Interrupt, MC9S12C128 Program
;*  Robert Beck March 22, 2017
;* 
;* 12 hour clock using RTI
;* This program is a 12 hour clock using 
;* a Real Time Interrupt service subroutine (RTIISR).  This program
;* displays the time remaining on the Hyper Terminal screen every 1 second.  
;* That is, this program displays '987654321098765432109876543210 . . . ' on the 
;* Hyper Terminal connected to MC9S12C128 chip on CSM-12C128 board.  
;* User may enter 'stop' command followed by an enter key to stop the timer 
;* and re-start the timer with 'run' command followed by an enter key.
;*
;* Please note the new feature of this program:
;* RTI vector, initialization of CRGFLG, CRGINT, RTICTL, registers for the
;* Real Time Interrupt.
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;* This program evaluates user input (command) after the enter key hit and allow 
;* maximum five characters for user input.  This program ignores the wrong 
;* user inputs, and continue count down.
;* 
;*******************************************************
; export symbols
            XDEF        Entry        ; export 'Entry' symbol
            ABSENTRY    Entry        ; for assembly entry point

; include derivative specific macros
SCISR1      EQU         $00cc        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00cf        ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG     $3000            ; RAMStart defined as $3000
                                     ; in MC9S12C128 chip

ctr2p5m     DS.W    1                ; 16bit interrupt counter for 2.5 mSec. of time
cbufct      DS.B    1                ; user input character buffer fill count
cbuf        DS.B    11               ; user input character buffer, maximum 10 char
cerror      DS.B    1                ; user input error count, 1 - 9 ($31 - $39)
disbuf      DS.B    10

times       DS.B    1                ; time to display on screen
timem       DS.B    1
timeh       DS.B    1
temp        DS.B    1



; following escape sequence works on Hyper Terminal (but NOT the TestTerminal)
ClearScreen     DC.B    $1B, '[2J', $00               ; clear the Hyper Terminal screen
ClearLine       DC.B    $1B, '[2K', $00               ; clear the line
Scroll4Enable   DC.B    $1B, '[1', $3B, '4r',  $00    ; enable scrolling to top 4 lines
UnSavePosition  DC.B    $1B, '8', $00                 ; restore the saved cursor position
SavePosition    DC.B    $1B, '7', $00                 ; save the current cursor position
CursorToTop     DC.B    $1B, '[2',  $3B, '1f',  $00   ; move cursor to 2,1 position
CursorToPP      DC.B    $1B, '[4',  $3B, '1f',  $00   ; move cursor to 4,1 position, prompt
CursorToCenter  DC.B    $1B, '[08', $3B, '25H', $00   ; move cursor to 8,25 position


;*******************************************************
; interrupt vector section

            ORG     $3FF0            ; Real Time Interrupt (RTI) interrupt vector setup
            DC.W    rtiisr

;*******************************************************
; code section
            ORG     $3100
Entry
            LDS     #Entry           ; initialize the stack pointer

            ldx     #ClearScreen     ; clear the Hyper Terminal Screen first
            jsr     printmsg
            ldx     #CursorToTop     ; and move the cursor to top left corner (2,1) to start
            jsr     printmsg
            ldx     #msg1            ; print the first message, 'Hello'
            jsr     printmsg
            jsr     nextline
            ldx     #msg2            ; print the second message, user instruction
            jsr     printmsg
            ldx     #Scroll4Enable   ; enable top 4 lines to scroll if necessary
            jsr     printmsg
            ldx     #CursorToPP      ; move cursor at (4,1) upper left corner for the prompt
            jsr     printmsg
            
            ldx     #398
            stx     ctr2p5m          ; initialize interrupt counter with 400.
            
            bset  RTICTL,%00011001   ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                     ;      4MHz quartz oscillator clock
            bset  CRGINT,%10000000   ; enable RTI interrupt
            bset  CRGFLG,%10000000   ; clear RTI IF (Interrupt Flag)
            ;clr   cbuf
          
            
            ldaa  #12                ; initialize the hour, min, and sec variables
            staa  timeh
            ldaa  #0
            staa  timem
            ldaa  #0
            staa  times
            cli

            
            
            
mLoop       jsr     nextline
            ldx     #prompt           ;print clock prompt
            jsr     printmsg
            
loop2       jsr     UpDisplay          ; update the display
            
            jsr   getchar            ; user may enter command
            cmpa  NULL
            beq   loop2
           

            staa  1,x+               ; save the user input character
;            inc   cbufct
            jsr   putchar            ; echo print, displayed on the terminal window

            cmpa  #CR                ; check for return key
            bne   loop2
            
            jsr   NewCommand         ; process new command
            bra   mLoop





;subroutine section below

;***********RTI interrupt service routine***************
rtiisr      bset  CRGFLG,%10000000   ; clear RTI Interrupt Flag
            ldx   ctr2p5m
            inx
            stx   ctr2p5m            ; every time the RTI occur, increase interrupt count
rtidone     RTI
;***********end of RTI interrupt service routine********

;***************Update Display**********************
;* Program: Update count down timer display if 1 second is up
;* Input:   ctr2p5m variable
;* Output:  timer display on the Hyper Terminal
;* Registers modified: CCR
;* Algorithm:
;    Check for 1 second passed
;      if not 1 second yet, just pass
;      if 1 second has reached, then update display, toggle LED, and reset ctr2p5m
;**********************************************
UpDisplay
            psha
            pshx
            ldx   ctr2p5m          ; check for 1 sec
            cpx   #399             ; 2.5msec * 400 = 1 sec        0 to 399 count is 400
            lblo   UpDone           ; if interrupt count less than 400, then not 1 sec yet.
                                   ;    no need to update display.
            ldx   #0               ; interrupt counter reached 400 count, 1 sec up now
            stx   ctr2p5m          ; clear the interrupt count to 0, for the next 1 sec.



            ldx   #SavePosition      ; save the current cursor posion (user input in
            jsr   printmsg           ;   progress at the prompt).
            ldx   #CursorToCenter    ; and move the cursor to center(relative) to print time
            jsr   printmsg

 ;           ldx   #cbuf
  ;          cpx   #0
   ;         beq   noReset
            
       

            ldab   timeh            ; convert hours to characters
            ldaa   #0
            ldx    #10
            idiv
            xgdx
            tba
            adda  #$30
            jsr   putchar            ; print the first digit
            xgdx
            tba
            adda  #$30
            jsr   putchar            ; and the second
            
            ldaa  #':'
            jsr   putchar            ; print colon
            
            ldab   timem
            ldaa  #0                 ; repeat process for minutes
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
            
            ldaa  #':'                 ; print colon
            jsr   putchar
            
            ldab   times               ; repeat process for seconds
            ldaa   #0
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
            
            ldx   #UnSavePosition    ; back to the prompt area for continued user input
            jsr   printmsg
            
            inc   times              ; increment seconds
            ldaa  times
            cmpa  #59
            ble   UpDone
            
            ldab  #0             ; reset if at 59
            stab  times
            
            inc   timem           ; check minutes
            ldaa  timem
            cmpa  #59
            ble   UpDone
            
            ldab  #0              ; reset if necessary
            stab  timem
            
            inc   timeh
            ldaa  timeh
            cmpa  #12               ; check hours 
            ble   UpDone
            
            ldab  #1                 ; reset if necessary
            stab  timeh
                       
UpDone      pulx                    ; end UpDisplay
            pula
            rts
;***************end of Update Display***************

;***************New Command Process*******************************
;* Program: Check for 'run' command or 'stop' command.
;* Input:   Command buffer filled with characters, and the command buffer character count
;*             cbuf, cbufct
;* Output:  Display on Hyper Terminal, count down characters 9876543210 displayed each 1 second
;*             continue repeat unless 'stop' command.
;*          When a command is issued, the count display reset and always starts with 9.
;*          Interrupt start with CLI for 'run' command, interrupt stops with SEI for 'stop' command.
;*          When a new command is entered, cound time always reset to 9, command buffer cleared, 
;*             print error message if error.  And X register pointing at the begining of 
;*             the command buffer.
;* Registers modified: X, CCR
;* Algorithm:
;*     check 'run' or 'stop' command, and start or stop the interrupt
;*     print error message if error
;*     clear command buffer
;*     Please see the flow chart.
;* 
;**********************************************
NewCommand
            psha
            
            ldx     #cbuf                 ; 
            ldaa    1,x+
            cmpa    #'q'
            lbeq    checkq
            
            cmpa    #'s'
            lbne     invcom
            
            
            
            ldaa  1,x+
            cmpa  #' '
            lbne   invform
            
            ldaa  1,x+
            cmpa  #$31
            lbgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            ldab  #10
            mul
            std   timeh
            
            ldaa  1,x+
            cmpa  #$39
            bgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            adda  timeh
            
            ldaa  1,x+
            cmpa  #':'
            bne   invform
            
            ldaa  1,x+
            cmpa  #$35
            bgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            ldab  #10
            mul
            std   timem
            
            ldaa  1,x+
            cmpa  #$39
            bgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            adda  timem
            
            ldaa  1,x+
            cmpa  #':'
            bne   invform
            
            ldaa  1,x+
            cmpa  #$35
            bgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            ldab  #10
            mul
            std   times
            
            ldaa  1,x+
            cmpa  #$39
            bgt   invform
            cmpa  #$30
            blt   invform
            suba  #$30
            adda  times
checkq           
            ldaa  1,x+
            cmpa  #CR
            bne   invform
            
            jsr     UpDisplay
            bra     exit
quit
            sei
            bra     exit
invcom
            jsr     nextline
            ldx     #errmsg1
            jsr     printmsg
            bra     exit
invform
            jsr     nextline
            ldx     #errmsg2
            jsr     printmsg    
exit
            ;clr     cbuf
            pula
            rts

   
;***************end of New Command Process***************


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
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
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
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                      ; send a character
            rts
;***************end of putchar*****************

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

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

msg2        DC.B    'You may type <s> or <q> command with enter key', $00
msg1        DC.B    'Hello, this is a clock program.', $00
msg3err     DC.B    'sys> Command Error, ', $00
msg4        DC.B    'CMD> ', $00
prompt      DC.B    'Clock> ', $00
errmsg1     DC.B    'invalid command', $00
errmsg2     DC.B    'invalid time format. correct example => hh:mm:ss', $00


            END                    ; this is end of assembly source file
                                   ; lines below are ignored - not assembled