;*****************************************************************
; PPU Registers : $2000 - $2007 and $4014

; The PPU has its own memory within the CPU known as Video RAM (VRAM), 
; where the PPU can allocate up to 64kb of memory, though the VRAM only has 16kb. 

; Reading and writing into the PPU is done by using the I/O addresses $2006 and $2007,
; where $2006 has two writes and $2007 read and write.
;*****************************************************************
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
; APU Registers 
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
; The Header indicates which mappers you are using and whether the cartidge contains SRAM for saving game progress. 
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
; Section of the code that searches for the needed interrupts 
;*****************************************************************
.segment "VECTORS"
.word nmi 	; Non-maskable interrupt, generated for every V-Blank that occurs at the end of each frame
.word reset ; Reset interrupt, triggered when the reset button is hit
.word irq 	; Maskable interrupt, triggered when a BRK occurs


;*****************************************************************
; 6502 Zero Page Memory (256 bytes)
;*****************************************************************
.segment "ZEROPAGE"



