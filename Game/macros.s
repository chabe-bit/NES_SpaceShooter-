;*****************************************************************
;                          Macros
;*****************************************************************
.macro vram_set_address newaddress

    ; > and < grabs the upper and lower bits of a 16-bit address
    ; Sets the PPU memory address in one call
    ; e.g. vram_set_address $2000

    lda PPU_STATUS
    lda #>newaddress
    sta PPU_VRAM_ADDRESS2
    lda #<newaddress
    sta PPU_VRAM_ADDRESS2

.endmacro

.macro assign_16i dest, value

    ; dest is the location in zero-page memory we want set 
    ; value is the 16-bit address to write

    lda #<value
    sta dest + 0
    lda #>value
    sta dest + 1

.endmacro

.macro vram_clear_address

  ; clears the PPU memory address
  
  lda #0
  sta PPU_VRAM_ADDRESS2
  sta PPU_VRAM_ADDRESS2

.endmacro

