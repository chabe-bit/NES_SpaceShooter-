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
;                        TILES 
;*****************************************************************
.segment "TILES"
.incbin "spaceshooter.chr"

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

.include "neslib.s"

;*****************************************************************
;              Sprite Object Attribute Memory (OAM) 
;*****************************************************************
.segment "OAM"
oam:       .res 256 

;*****************************************************************
;                        BSS (RAM)
;*****************************************************************
.segment "BSS" ; RAM
palette: .res 32 ; Current palette buffer

;*****************************************************************
;                          IRQ
;*****************************************************************
.segment "CODE"
irq: rti

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

;*****************************************************************
;                          NMI
;*****************************************************************
.segment "ZEROPAGE"

time: .res 1
lasttime: .res 1

.segment "CODE"
.proc nmi
    pha 
    txa
    pha
    tya
    pha

    inc time

    bit PPU_STATUS
    lda #>oam
    sta SPRITE_DMA

    vram_set_address $3F00
    ldx #0

@loop:
    lda palette, x 
    sta PPU_VRAM_IO
    inx
    cpx #32
    bcc @loop

    lda #0
    sta PPU_VRAM_ADDRESS1
    sta PPU_VRAM_ADDRESS1
    lda ppu_ctl0
    sta PPU_CONTROL
    lda ppu_ctl1
    sta PPU_MASK

    ldx #0
    stx nmi_ready
    pla 
    tay 
    pla 
    tax 
    pla 
    rti
.endproc


;*****************************************************************
;                       ENTRY POINT
;*****************************************************************
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
;                         WRITE TEXT  
;*****************************************************************
.segment "ZEROPAGE"

text_address: .res 2

.segment "CODE"
.proc write_text
    ldy #0

loop: 
    lda (text_address), y
    beq exit
    sta PPU_VRAM_IO
    iny
    jmp loop
exit:
    rts

.endproc

;*****************************************************************
;                         TITLE SCREEN 
;*****************************************************************
.segment "ZEROPAGE"

paddr: .res 2

.segment "CODE"
title_text:
.byte "M E G A  B L A S T", 0

press_play_text:
.byte "PRESS FIRE TO START", 0

title_attributes:
.byte %00000101, %00000101, %00000101, %00000101
.byte %00000101, %00000101, %00000101, %00000101

.proc display_title_screen
    jsr ppu_off 

    jsr clear_nametable

    vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 6)
    assign_16i text_address, title_text
    jsr write_text

    vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 6)
    assign_16i text_address, press_play_text
    jsr write_text

    vram_set_address (ATTRIBUTE_TABLE_0_ADDRESS + 8)
    assign_16i paddr, title_attributes
    ldy #0

loop:
    lda (paddr), Y
    sta PPU_VRAM_IO
    iny 
    cpy #8
    bne loop 

    jsr ppu_update

    rts
.endproc

;*****************************************************************
;                         GAME SCREEN 
;*****************************************************************
.segment "RODATA"

; Store our mountain data in our ROM
game_screen_mountain: 
.byte 001, 002, 003, 004, 001, 002, 003, 004, 001, 002, 003, 004, 001, 002, 003, 004
.byte 001, 002, 003, 004, 001, 002, 003, 004, 001, 002, 003, 004, 001, 002, 003, 004

; Store our score line in our ROM
game_scren_scoreline:
.byte "SCORE 0000000"

.segment "CODE"
.proc display_game_screen
    jsr ppu_off ; Turn off the PPU once the frame is done drawing

    jsr clear_nametable ; Clear name table 0

    vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32) ; Output the mountain line 
    assign_16i paddr, game_screen_mountain ; Set the address of the the mountain into our physical address
    ldy #0 ; Set y register to 0 

loop:
    lda (paddr), y  
    sta PPU_VRAM_IO
    iny 
    cpy #32
    bne loop 

    vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32)
    ldy #0
    lda #9

loop2:
    sta PPU_VRAM_IO
    iny 
    cpy #32 
    bne loop2 

    assign_16i paddr, game_scren_scoreline
    ldy #0

loop3:
    lda (paddr), y 
    sta PPU_VRAM_IO
    iny 
    cpy #13
    bne loop3

    jsr ppu_update
    rts
.endproc

;*****************************************************************
;                      PLAYER MOVEMENT
;*****************************************************************
.segment "CODE"
.proc player_actions
    jsr gamepad_poll
    lda gamepad 
    and #PAD_L
    beq not_gamepad_left 
        lda oam + 3
        cmp #0
        beq not_gamepad_left
        sec 
        sbc #2 

        sta oam + 3
        sta oam + 11 
        clc 
        adc #8
        sta oam + 7
        sta oam + 15

not_gamepad_left:
    lda gamepad 
    and #PAD_R
    beq not_gamepad_right
        lda oam + 3
        clc 
        adc #12 
        cmp #254 
        beq not_gamepad_right
        lda oam + 3
        clc 
        adc #2 

        sta oam + 3
        sta oam + 11 
        clc 
        adc #8 
        sta oam + 7 
        sta oam + 15

not_gamepad_right:  
    lda gamepad 
    and #PAD_A 
    beq not_gamepad_a
        lda oam + 16
        cmp #$FF
        bne not_gamepad_a
        lda #192
        sta oam + 16
        lda #4
        sta oam + 17
        lda #0
        sta oam + 18
        lda oam + 3
        clc 
        adc #6 
        sta oam + 19

not_gamepad_a:

    rts
.endproc

;*****************************************************************
;                        PLAYER SHOOT
;*****************************************************************
.segment "CODE"
.proc move_player_shoot
    lda oam + 16 
    cmp #$FF
    beq @exit 
        sec 
        sbc #4 
        sta oam + 16 
        bcs @exit 
            lda #$FF
            sta oam + 16 

@exit:
    rts

.endproc

;*****************************************************************
;                           MAIN 
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

    jsr display_title_screen

    lda #VBLANK_NMI | BG_0000 | OBJ_1000
        sta ppu_ctl0
        lda #BG_ON | OBJ_ON
        sta ppu_ctl1
    
    jsr ppu_update

titleloop:
    jsr gamepad_poll
    lda gamepad 
    and #PAD_A | PAD_B | PAD_START | PAD_SELECT
    beq titleloop

    jsr display_game_screen

    lda #192 

    sta oam
    sta oam + 4
    lda #200
    sta oam + 8 
    sta oam + 12 

    ldx #0 
    stx oam + 1
    inx 
    stx oam + 5
    inx 
    stx oam + 9
    inx 
    stx oam + 13 

    lda #%00000000
    sta oam + 2
    sta oam + 6 
    sta oam + 10 
    sta oam + 14 

    lda #120
    sta oam + 3 
    sta oam + 11 
    lda #128
    sta oam + 7
    sta oam + 15 

    jsr ppu_update
mainloop:
    lda time 

    cmp lasttime 
    beq mainloop 
    
    sta lasttime 

    jsr player_actions
    jsr move_player_shoot

    jmp mainloop 
.endproc