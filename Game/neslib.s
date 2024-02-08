;*****************************************************************
;                    NES Function Library
;*****************************************************************

;*****************************************************************
;                    Registers/Addresses
;*****************************************************************
; PPU Registers
PPU_CONTROL 		= $2000 ; Write
PPU_MASK 			= $2001 ; Write
PPU_STATUS 			= $2002 ; Read
PPU_SPRRAM_ADDRESS  = $2003 ; Write
PPU_SPRRAM_IO 		= $2004 ; Write
PPU_VRAM_ADDRESS1 	= $2005 ; Write
PPU_VRAM_ADDRESS2 	= $2006 ; Write
PPU_VRAM_IO 		= $2007 ; Read/Write
SPRITE_DMA 			= $4014 ; Write

; Nametable addresses
NT_2000 = $00
NT_2400 = $01
NT_2800 = $02
NT_2C00 = $03

; Increment vram pointer by row
VRAM_DOWN = $04

; Sprite locations
OBJ_0000 = $00
OBJ_1000 = $08
OBJ_8X16 = $20

; Background locations
BG_0000 = $00
BG_1000 = $10

; Enable NMI
VBLANK_NMI = $80

; Background options
BG_OFF  = $00
BG_CLIP = $08
BG_ON   = $0A

; Sprite options
OBJ_OFF  = $00
OBJ_CLIP = $10
OBJ_ON   = $14

; API Registers
APU_DM_CONTROL = $4010
APU_CLOCK = $4015

; Controllers
JOYPAD1 = $4016
JOYPAD2 = $4017

; Controller input
PAD_A = $01
PAD_B = $02
PAD_START  = $04
PAD_SELECT = $08
PAD_U = $10
PAD_D = $20
PAD_L = $40
PAD_R = $80

; Addresses for the PPU
NAME_TABLE_0_ADDRESS = $2000
ATTRIBUTE_TABLE_0_ADDRESS = $23C0
NAME_TABLE_1_ADDRESS = $2400
ATTRIBUTE_TABLE_1_ADDRESS = $27C0


;*****************************************************************
;                          Functions
;*****************************************************************
.segment "ZEROPAGE"
ppu_ctl0:		.res 1 ; PPU Control Register 2 Value
ppu_ctl1:		.res 1 ; PPU Control Register 2 Value

.include "macros.s"

; At the end of the NMI routine, this function is called to rest the nmi_update flag
.segment "CODE"
.proc wait_frame
    inc nmi_ready
@loop:
    lda nmi_ready
    bne @loop
    rts
.endproc

;*****************************************************************
;                       ENABLE PPU  
;*****************************************************************
; Turns on rendering at the start of the next NMI routine
.segment "CODE"
.proc ppu_update
    lda ppu_ctl0 
    ora #VBLANK_NMI 
    sta ppu_ctl0
    sta PPU_CONTROL
    lda ppu_ctl1
    ora #OBJ_ON | BG_ON
    sta ppu_ctl1
    jsr wait_frame
    rts
.endproc

;*****************************************************************
;                       DISABLE PPU    
;*****************************************************************
; Turns off rendering at the end of the the current render
.segment "CODE"
.proc ppu_off
    jsr wait_frame
    lda ppu_ctl0
    and #%01111111
    sta ppu_ctl0
    sta PPU_CONTROL
    lda ppu_ctl1
    and #%11100001
    sta ppu_ctl1
    sta PPU_MASK
    rts
.endproc

;*****************************************************************
;                     CLEAR NAMETABLE  
;*****************************************************************
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
;                      GAMEPAD POLL  
;*****************************************************************
.segment "CODE"
.proc gamepad_poll
    lda #1
    sta JOYPAD1
    lda #0
    sta JOYPAD1
    ldx #8

    loop:
        pha
        lda JOYPAD1
        ; Combine two low bits into carry bit
        and #%00000011
        cmp #%00000001
        pla 
        
        ; Rotate carry into gamepad
        ror a
        dex
        bne loop
        sta gamepad
        rts
.endproc