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
irq: 
    rti

;*****************************************************************
;                          NMI
;*****************************************************************
.segment "CODE"
.proc nmi
    pha
    txa
    pha
    tya
    pha

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

.segment "CODE"
.proc main
    ldx #0

paletteloop:
    lda default_palette, x 
    sta palette, x 
    inx 
    cpx #32
    bcc paletteloop

    lda #VBLANK_NMI | BG_0000 | OBJ_1000
        sta ppu_ctl0
        lda #BG_ON | OBJ_ON
        sta ppu_ctl1
    
    jsr ppu_update

mainloop:
    jmp mainloop
.endproc
    
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