;*****************************************************************
;                           PPU
;*****************************************************************

; Addresses $2000 - $2007 and $4014
; The PPU has its own memory within the CPU known as Video RAM (VRAM), 
; where the PPU can allocate up to 64kb of memory, though the VRAM only has 16kb. 

; Reading and writing into the PPU is done by using the I/O addresses $2006 and $2007,
; where $2006 has two writes and $2007 read and write.

PPU_CONTROL 		= $2000 ; Write
PPU_MASK 			= $2001 ; Write
PPU_STATUS 			= $2002 ; Read
PPU_SPRRAM_ADDRESS  = $2003 ; Write
PPU_SPRRAM_IO 		= $2004 ; Write
PPU_VRAM_ADDRESS1 	= $2005 ; Write
PPU_VRAM_ADDRESS2 	= $2006 ; Write
PPU_VRAM_IO 		= $2007 ; Read/Write
SPRITE_DMA 			= $4014 ; Write

;*****************************************************************
;                           APU
;*****************************************************************
APU_DM_CONTROL 		= $4010
APU_CLOCK 			= $4015

JOYPAD1 			= $4016 ; Read/Write
JOYPAD2 			= $4017 ; Read/Write

PAD_A 				= $01 
PAD_B 				= $02
PAD_SELECT 			= $04
PAD_START 			= $08
PAD_U 				= $10
PAD_D 				= $20
PAD_L 				= $40
PAD_R 				= $80

;*****************************************************************
;                        HEADER 
;*****************************************************************
.segment "HEADER" 
INES_MAPPER 		= 0 ; Which NES Memory Mapper to use
INES_MIRROR 		= 0 ; Whether the game is mirrored vertically or horizontally
INES_SRAM   		= 0 ; Set true if the cartridge contains an SRAM, otherwise false

.byte 'N', 'E', 'S', $1A 
.byte $02 				; $02 indicates 16k - PRG bank count
.byte $01				; $01 indicates 8k  - CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4) 
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; Padding

;*****************************************************************
;                        VECTORS 
;*****************************************************************
.segment "VECTORS"
.word nmi   ; Non-maskable interrupt, generated for every V-Blank that occurs at the end of each frame
.word reset ; Reset interrupt, triggered when the reset button is hit
.word irq   ; Maskable interrupt, triggered when a BRK occurs

;*****************************************************************
;                       ZERO PAGE
;*****************************************************************
.segment "ZEROPAGE"
nmi_ready:  .res 1 ; 1 updates the frame | 2 turn rendering off
gamepad:    .res 1 ; Stores the current gamepad values
d_x:        .res 1 ; X velocity of ball
d_y:        .res 1 ; Y velocity of ball

;*****************************************************************
;              Sprite Object Attribute Memory (OAM) 
;*****************************************************************
.segment "OAM"
oam:       .res 256 

;*****************************************************************
;                        READ ONLY DATA
;*****************************************************************
.segment "RODATA"
default_palette:
.byte $0F, $15, $26, $37 ; background 0 set to purple/pink
.byte $0F, $09, $19, $29 ; background 1 set to green
.byte $0F, $01, $11, $21 ; background 2 set to blue
.byte $0F, $00, $10, $30 ; backgtound 3 set to grey
.byte $0F, $18, $28, $38 ; sprite 0 set to yellow
.byte $0F, $14, $24, $34 ; sprite 1 set to purple 
.byte $0F, $1B, $2B, $3B ; sprite 2 set to teal
.byte $0F, $12, $22, $32 ; sprite 3 set to marine

welcome_txt:
.byte 'W', 'E', 'L', 'C', 'O', 'M', 'E', 0

;*****************************************************************
;                        TILES (ROM)
;*****************************************************************
.segment "TILES" ; Stored into ROM data
.incbin "example.chr" ; include binary file that is copied into our final NES ROM

;*****************************************************************
;                        BSS (RAM)
;*****************************************************************
.segment "BSS" ; RAM
palette: .res 32 ; Current palette buffer

;*****************************************************************
;                          IRQ
;*****************************************************************
.segment "CODE"
irq: 
    rti

;*****************************************************************
;                       ENTRY POINT
;*****************************************************************
; In order to initialize the NES, we first need to make sure nothing is on, that everything is cleared. 
; The PPU takes 29658 cycles before it's ready for any commands, so it's good practice to use a loop to wait 

; First, we turn off all normal interrupts
; Second, disable NMI signals
; Third, disable the PPU 
; Fourth, disable direct memory transfers
; Fifth, disable the APU

.segment "CODE"
.proc reset ; .proc is to indicate the start of our code / .endproc is the end of our code
    sei                  ; Clears the interrupt flag
    lda #0               ; Load value 0 into the accumulator that allocates sta to be 0
    sta PPU_CONTROL      ; Disable NMI
    sta PPU_MASK         ; Disable PPU Rendering
    sta APU_DM_CONTROL   ; Disable APU
    lda #$40             ; Load value 40 into the accumulator for the 2nd control pad, which is part of the APU
    sta JOYPAD2          ; Disable APU fram interrupt (IRQ)

    cld                  ; Clear flag
    ldx #$FF             ; Set X register to 255
    txs                  ; Transfer X to S register

    ; Wait for the first v blank
    bit PPU_STATUS 
wait_vblank:
    bit PPU_STATUS
    bpl wait_vblank

    lda #0
    ldx #0

; The X register has 8 bits, set each to 0
clear_ram: 
    sta $0000, x 
    sta $0100, x 
    sta $0200, x 
    sta $0300, x         
    sta $0400, x 
    sta $0500, x 
    sta $0600, x 
    sta $0700, x
    inx
    bne clear_ram ; BNE executes if the Z register is cleared or empty

    lda #255
    ldx #0
clear_oam:
    sta oam, x 
    inx  
    inx
    inx
    inx
    bne clear_oam

; Wait for the second v blank
; Vertical blank. The blink before each frame is rendered.
wait_vblank2:
    bit PPU_STATUS
    bpl wait_vblank2

    lda #%10001000
    sta PPU_CONTROL

    jmp main
        
.endproc

;*****************************************************************
;                     NMI ROUTINE
;*****************************************************************

; A frame is rendered row by row, from left to right, on a 256 x 240 pixel screen
; Vblank is an interrupt that occurs every 60fps on the NTSC and 50fps on the PAL. 
; After the vblank. the NMI routine is called, where any changes you'd want to render should be

; NMI routine
.segment "CODE"
.proc nmi
    pha ; push value into a register
    txa ; transfer value into x register
    pha 
    tya ; transfer value into y register
    pha

    lda nmi_ready
    bne :+
        jmp ppu_update_end
    :
    cmp #2
    bne cont_render
        lda #%00000000
        sta PPU_MASK
        ldx #0
        stx nmi_ready
        jmp ppu_update_end
    
cont_render:
    ldx #0
    stx PPU_SPRRAM_ADDRESS
    lda #>oam
    sta SPRITE_DMA

    ; Transfer current palette to the PPU
    lda #%10001000 ; Set horizontal nametable increment
    sta PPU_CONTROL 
    lda PPU_STATUS
    lda #$3F ; Set PPU address 
    sta PPU_VRAM_ADDRESS2 
    stx PPU_VRAM_ADDRESS2
    ldx #0 ; Transfer the 32 ytes to VRAM

loop:
    lda palette, x
    sta PPU_VRAM_IO
    inx
    cpx #32
    bcc loop

    lda #%00011110
    sta PPU_MASK
    
    ldx #0
    stx nmi_ready

ppu_update_end:
    pla
    tay 
    pla
    tax
    pla
    rti
.endproc    

;*****************************************************************
;                    ENABLE PPU
;*****************************************************************

; Enables the PPU for rendering, then waits for the next NMI
.segment "CODE"
.proc ppu_update
    lda #1
    sta nmi_ready
    
    loop: 
        lda nmi_ready
        bne loop

    rts
.endproc

;*****************************************************************
;                    DISABLE PPU
;*****************************************************************

; Disables the PPU in the case you need to transfer a large amount of data to the PPU's ram
.segment "CODE"
.proc ppu_off
    lda #2
    sta nmi_ready
    
    loop:
        lda nmi_ready
        bne loop
    
    rts
.endproc

;*****************************************************************
;                    CLEAR NAMETABLE
;*****************************************************************

; Clears the name tables to clear the screen 
.segment "CODE"
.proc clear_nametable
    lda PPU_STATUS 
    lda #$20
    sta PPU_VRAM_ADDRESS2
    lda #$00
    sta PPU_VRAM_ADDRESS2

    ; Clearing the name table 
    lda #0
    ldy #30 ; Iterate over 30 rows
    rowloop:    
        ldx #32 ; Iterate over 32 cols 
        columnloop:
            sta PPU_VRAM_IO 
            dex 
            bne columnloop
        dey
        bne rowloop
    
    ldx #64
    loop:
        sta PPU_VRAM_IO
        dex
        bne loop
    rts
.endproc

;*****************************************************************
;                    READ CONTROLLER INPUT
;*****************************************************************

; Each NES gamepad is polled seperately or one at a time.
; We read input from the gamepad by first writing int 1 and 0. 
; Then we read the input 8 times, 8 time because it's the amount of buttons. 
; Each button is given a bit. 
; We only want the first two bits, thus we use the value %00000011.
; Then we compare that value using the AND operator to value %00000001

.segment "CODE"
.proc gamepad_poll
    lda #1
    sta JOYPAD1
    lda #0
    sta JOYPAD2
    ldx #8

    loop:
        pha
        lda JOYPAD1
        ; Combine two low bits into carry bit
        and #%00000011
        cmp #%00000001
        pla 
        
        ; Rotate carry into gamepad
        ror
        dex
        bne loop
        sta gamepad
        rts
.endproc

;*****************************************************************
;                    MAIN LOOP
;*****************************************************************
.segment "CODE"
.proc main
    ldx #0

    paletteloop:
        lda default_palette, x
        sta palette, x 
        inx 
        cpx #32
        bcc paletteloop

        jsr clear_nametable

        lda PPU_STATUS
        lda #$20
        sta PPU_VRAM_ADDRESS2
        lda #$8A
        sta PPU_VRAM_ADDRESS2

        ldx #0

    textloop:
        lda welcome_txt, x 
        sta PPU_VRAM_IO
        inx 
        cmp #0
        beq :+
            jmp textloop
        :
        ; Placing our bat on the screen
        lda #180
        sta oam 
        lda #120
        sta oam + 3
        lda #1
        sta oam + 1
        lda #0
        sta oam + 2

        ; Placing our ball on the screen 
        lda #124
        sta oam + (1 * 4) ; Set sprite to Y pos
        sta oam + (1 * 4) + 3 ; Set sprite to X pos
        lda #2 
        sta oam + (1 * 4) + 1 ; Set sprite one pattern 
        lda #0
        sta oam + (1 * 4) + 2 ; Set sprite one attributes
    
        ; Set the ball velocity
        lda #1
        sta d_x
        sta d_y

        ; Render the screen
        jsr ppu_update

    mainloop:
        lda nmi_ready
        cmp #0
        bne mainloop

        jsr gamepad_poll
        lda gamepad
        and #PAD_L
        beq NOT_GAMEPAD_LEFT
            lda oam + 3
            cmp #3
            beq NOT_GAMEPAD_LEFT
            sec 
            sbc #1
            sta oam + 3

    NOT_GAMEPAD_LEFT:
        lda gamepad
        and #PAD_R
        beq NOT_GAMEPAD_RIGHT
            lda oam + 3
            cmp #248
            beq NOT_GAMEPAD_RIGHT
            clc 
            adc #1
            sta oam + 3
     NOT_GAMEPAD_RIGHT:
 	; now move our ball
 	lda oam + (1 * 4) + 0 ; get the current Y
	clc
	adc d_y ; add the Y velocity
 	sta oam + (1 * 4) + 0 ; write the change
 	cmp #0 ; have we hit the top border
 	bne NOT_HITTOP
 		lda #1 ; reverse direction
 		sta d_y
    NOT_HITTOP:
        lda oam + (1 * 4) + 0
        cmp #210 ; have we hit the bottom border
        bne NOT_HITBOTTOM
            lda #$FF ; reverse direction (-1)
            sta d_y
    NOT_HITBOTTOM:
        lda oam + (1 * 4) + 3 ; get the current x
        clc
        adc d_x	; add the X velocity
        sta oam + (1 * 4) + 3
        cmp #0 ; have we hit the left border
        bne NOT_HITLEFT
            lda #1 ; reverse direction
            sta d_x
    NOT_HITLEFT:
        lda oam + (1 * 4) + 3
        cmp #248 ; have we hot the right border
        bne NOT_HITRIGHT
            lda #$FF ; reverse direction (-1)
            sta d_x
    NOT_HITRIGHT:

        ; ensure our changes are rendered
        lda #1
        sta nmi_ready
        jmp mainloop
.endproc

        

