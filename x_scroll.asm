
 processor 6502
	org $801
StartBlock801:
	; Starting new memory block at $801
	.byte $b ; lo byte of next line
	.byte $8 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $32,$30,$36,$34
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $801
EndBlock801:
	org $810
StartBlock810:
	; Starting new memory block at $810
x_scroll
	; LineNumber: 227
	jmp block1
	; LineNumber: 34
	; LineNumber: 35
	; LineNumber: 37
from_ptr	= $02
	; LineNumber: 37
to_ptr	= $04
	; LineNumber: 37
screen_base_ptr	= $08
	; LineNumber: 37
backbuffer_base_ptr	= $16
	; LineNumber: 37
map_ptr	= $0B
	; LineNumber: 39
map_column	dc.w	$00
	; LineNumber: 41
current_screen	dc.b	$00
	; LineNumber: 41
i	dc.b	$00
	; LineNumber: 42
row	dc.b	$00
	; LineNumber: 42
numlines	dc.b	$00
	; LineNumber: 42
startline	dc.b	$00
	; LineNumber: 44
scroll	dc.b	$07
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToBack
	;    Procedure type : User-defined procedure
	rts
end_procedure_DrawColumn39ToBack
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4c
mul16x8_num1 = $4e
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
end_procedure_init16x8mul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4c
multiplier_a = $4e
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4e
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit2
	rts
end_procedure_initeightbitmul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : screen_swap
	;    Procedure type : User-defined procedure
	rts
end_procedure_screen_swap
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_line_65
	;    Procedure type : User-defined procedure
	; LineNumber: 57
irq_line_65
	; LineNumber: 59
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 63
	
; //SetMultiColorMode();
; //startline := 0;
; //numlines := 12;
; //copy_colors(); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock7
irq_line_65_ConditionalTrueBlock5: ;Main true block ;keep 
	; LineNumber: 64
	; LineNumber: 68
irq_line_65_elsedoneblock7
	; LineNumber: 70
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 72
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 74
	rti
end_procedure_irq_line_65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 99
irq_begin_vblank
	; LineNumber: 101
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 103
	; Test Inc dec D
	dec scroll
	; LineNumber: 105
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock13
irq_begin_vblank_ConditionalTrueBlock12: ;Main true block ;keep 
	; LineNumber: 105
	; LineNumber: 108
	
; //	addbreakpoint();
	jsr swap_screens
	; LineNumber: 110
	jmp irq_begin_vblank_elsedoneblock14
irq_begin_vblank_elseblock13
	; LineNumber: 111
	; LineNumber: 113
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 116
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock34
irq_begin_vblank_ConditionalTrueBlock32: ;Main true block ;keep 
	; LineNumber: 117
	; LineNumber: 118
	
; // Copy top half of char screen to back buffer.
	lda #$4
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 119
	
; // zero-based
	lda #$8
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 120
	
; // one based
	jsr copy_and_shift
	; LineNumber: 121
irq_begin_vblank_elsedoneblock34
	; LineNumber: 124
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock40
irq_begin_vblank_ConditionalTrueBlock38: ;Main true block ;keep 
	; LineNumber: 125
	; LineNumber: 126
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 127
	lda #$9
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 128
	jsr copy_and_shift
	; LineNumber: 129
irq_begin_vblank_elsedoneblock40
	; LineNumber: 130
irq_begin_vblank_elsedoneblock14
	; LineNumber: 132
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 133
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 136
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 138
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 141
copy_and_shift
	; LineNumber: 143
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock46
copy_and_shift_ConditionalTrueBlock45: ;Main true block ;keep 
	; LineNumber: 144
	; LineNumber: 145
	lda #$00
	ldx #$30
	sta from_ptr
	stx from_ptr+1
	; LineNumber: 146
	ldx #$34
	sta to_ptr
	stx to_ptr+1
	; LineNumber: 148
	jmp copy_and_shift_elsedoneblock47
copy_and_shift_elseblock46
	; LineNumber: 149
	; LineNumber: 150
	lda #$00
	ldx #$34
	sta from_ptr
	stx from_ptr+1
	; LineNumber: 151
	ldx #$30
	sta to_ptr
	stx to_ptr+1
	; LineNumber: 152
copy_and_shift_elsedoneblock47
	; LineNumber: 154
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy from_ptr+1 ; keep
	lda from_ptr
copy_and_shift_rightvarInteger_var54 = $54
	sta copy_and_shift_rightvarInteger_var54
	sty copy_and_shift_rightvarInteger_var54+1
	; Generic 16 bit op
	ldy #0
	lda #$1
copy_and_shift_rightvarInteger_var57 = $56
	sta copy_and_shift_rightvarInteger_var57
	sty copy_and_shift_rightvarInteger_var57+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda startline
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var57
copy_and_shift_wordAdd55
	sta copy_and_shift_rightvarInteger_var57
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var57+1
	tay
	lda copy_and_shift_rightvarInteger_var57
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var54
copy_and_shift_wordAdd52
	sta copy_and_shift_rightvarInteger_var54
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var54+1
	tay
	lda copy_and_shift_rightvarInteger_var54
	sta from_ptr
	sty from_ptr+1
	; LineNumber: 155
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy to_ptr+1 ; keep
	lda to_ptr
copy_and_shift_rightvarInteger_var60 = $54
	sta copy_and_shift_rightvarInteger_var60
	sty copy_and_shift_rightvarInteger_var60+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda startline
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var60
copy_and_shift_wordAdd58
	sta copy_and_shift_rightvarInteger_var60
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var60+1
	tay
	lda copy_and_shift_rightvarInteger_var60
	sta to_ptr
	sty to_ptr+1
	; LineNumber: 157
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 158
copy_and_shift_while61
copy_and_shift_loopstart65
	; Binary clause Simplified: LESS
	lda row
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs copy_and_shift_elsedoneblock64
copy_and_shift_ConditionalTrueBlock62: ;Main true block ;keep 
	; LineNumber: 159
	; LineNumber: 162
	; memcpyfast
	ldy #38
copy_and_shift_memcpy72
	lda (from_ptr),y
	sta (to_ptr),y
	dey
	bpl copy_and_shift_memcpy72
	; LineNumber: 167
	lda from_ptr
	clc
	adc #$28
	sta from_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd73
	inc from_ptr+1
copy_and_shift_WordAdd73
	; LineNumber: 168
	lda to_ptr
	clc
	adc #$28
	sta to_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd74
	inc to_ptr+1
copy_and_shift_WordAdd74
	; LineNumber: 169
	; Test Inc dec D
	inc row
	; LineNumber: 170
	jmp copy_and_shift_while61
copy_and_shift_elsedoneblock64
copy_and_shift_loopend66
	; LineNumber: 171
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39FromMap
	;    Procedure type : User-defined procedure
	; LineNumber: 174
DrawColumn39FromMap
	; LineNumber: 176
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne DrawColumn39FromMap_elseblock78
DrawColumn39FromMap_ConditionalTrueBlock77: ;Main true block ;keep 
	; LineNumber: 176
	; LineNumber: 178
	lda #$00
	ldx #$34
	sta to_ptr
	stx to_ptr+1
	; LineNumber: 180
	jmp DrawColumn39FromMap_elsedoneblock79
DrawColumn39FromMap_elseblock78
	; LineNumber: 181
	; LineNumber: 182
	lda #$00
	ldx #$30
	sta to_ptr
	stx to_ptr+1
	; LineNumber: 183
DrawColumn39FromMap_elsedoneblock79
	; LineNumber: 187
	
; // 4 blank rows, then 17 map rows, then 4 blank rows
; // Base + column offset +(row offset * 512)
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 189
	lda to_ptr
	clc
	adc #$a0
	sta to_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd84
	inc to_ptr+1
DrawColumn39FromMap_WordAdd84
	; LineNumber: 190
	
; // Start on screen row 4
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy map_column+1 ; keep
	lda map_column
DrawColumn39FromMap_rightvarInteger_var87 = $54
	sta DrawColumn39FromMap_rightvarInteger_var87
	sty DrawColumn39FromMap_rightvarInteger_var87+1
	; Integer constant assigning
	ldy #$50
	lda #$00
	; Low bit binop:
	clc
	adc DrawColumn39FromMap_rightvarInteger_var87
DrawColumn39FromMap_wordAdd85
	sta DrawColumn39FromMap_rightvarInteger_var87
	; High-bit binop
	tya
	adc DrawColumn39FromMap_rightvarInteger_var87+1
	tay
	lda DrawColumn39FromMap_rightvarInteger_var87
	sta map_ptr
	sty map_ptr+1
	; LineNumber: 201
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
DrawColumn39FromMap_forloop88
	; LineNumber: 193
	; LineNumber: 195
	; Load pointer array
	ldy #$0
	lda (map_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (to_ptr),y
	; LineNumber: 196
	lda to_ptr
	clc
	adc #$28
	sta to_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd97
	inc to_ptr+1
DrawColumn39FromMap_WordAdd97
	; LineNumber: 197
	lda map_ptr
	clc
	adc #$00
	sta map_ptr+0
	lda map_ptr+1
	adc #$02
	sta map_ptr+1
	; LineNumber: 200
DrawColumn39FromMap_forloopcounter90
DrawColumn39FromMap_loopstart91
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$11
	cmp i ;keep
	bcs DrawColumn39FromMap_forloop88
DrawColumn39FromMap_loopdone99: ;keep
DrawColumn39FromMap_forloopend89
DrawColumn39FromMap_loopend92
	; LineNumber: 202
	lda map_column
	clc
	adc #$01
	sta map_column+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd100
	inc map_column+1
DrawColumn39FromMap_WordAdd100
	; LineNumber: 203
	; Binary clause INTEGER: EQUALS
	; Compare INTEGER with pure num / var optimization. GREATER. 
	lda map_column+1   ; compare high bytes
	cmp #$00 ;keep
	bne DrawColumn39FromMap_elsedoneblock104
	lda map_column
	cmp #$fe ;keep
	bne DrawColumn39FromMap_elsedoneblock104
	jmp DrawColumn39FromMap_ConditionalTrueBlock102
DrawColumn39FromMap_ConditionalTrueBlock102: ;Main true block ;keep 
	; LineNumber: 202
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta map_column
	sty map_column+1
DrawColumn39FromMap_elsedoneblock104
	; LineNumber: 205
	rts
end_procedure_DrawColumn39FromMap
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 208
swap_screens
	; LineNumber: 211
	
; //addbreakpoint();
	jsr DrawColumn39FromMap
	; LineNumber: 212
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 213
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 215
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda current_screen
	clc
	adc #$1
	 ; end add / sub var with constant
	and #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 216
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne swap_screens_elseblock110
swap_screens_ConditionalTrueBlock109: ;Main true block ;keep 
	; LineNumber: 215
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	jmp swap_screens_elsedoneblock111
swap_screens_elseblock110
	; LineNumber: 215
	lda $d018
	and #%00001111
	ora #192
	sta $d018
swap_screens_elsedoneblock111
	; LineNumber: 218
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 224
	rts
end_procedure_swap_screens
block1
main_block_begin_
	; LineNumber: 228
	
; //addbreakpoint();
; //copy_colors(1); 
; // color_shift_lower
	sei
	; LineNumber: 231
	
; // System IRQs, not mine.
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 232
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 233
	; Clear screen with offset
	lda #$1
	ldx #$fa
MainProgram_clearloop116
	dex
	sta $0000+$3000,x
	sta $00fa+$3000,x
	sta $01f4+$3000,x
	sta $02ee+$3000,x
	bne MainProgram_clearloop116
	; LineNumber: 233
	; Clear screen with offset
	lda #$1
	ldx #$fa
MainProgram_clearloop117
	dex
	sta $0000+$3400,x
	sta $00fa+$3400,x
	sta $01f4+$3400,x
	sta $02ee+$3400,x
	bne MainProgram_clearloop117
	; LineNumber: 236
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d020
	; LineNumber: 237
	; Assigning memory location
	lda #$1
	; Calling storevariable on generic assign expression
	sta $d021
	; LineNumber: 238
	lda #$7
	; Calling storevariable on generic assign expression
	sta $D021+$1
	; LineNumber: 239
	lda #$8
	; Calling storevariable on generic assign expression
	sta $D021+$2
	; LineNumber: 240
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 247
	
; //	screen_bg_col:=black;
; //	screen_fg_col:=0;
; //	screen_fg_col[2]:=dark_grey;
; //screen_fg_col[1]:=grey;
; //poke(^$d018, 0, $17);	
; // Lower\upper chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 248
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 251
	
; //fillwithchar_slow();
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 253
	
; // Do this last.
	lda $d018
	and #%11110001
	ora #8
	sta $d018
	; LineNumber: 254
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	; LineNumber: 255
	jmp * ; loop like (�/%
	; LineNumber: 257
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:
	org $2000
StartBlock2000:
	org $2000
charset:
	incbin	 "C:/Users/alanp/dev/trse/trse-scroller///resources/UridiumChars.bin"
end_incbin_charset:
EndBlock2000:
	org $5000
StartBlock5000:
	org $5000
map:
	incbin	 "C:/Users/alanp/dev/trse/trse-scroller///resources/UridiumMap.bin"
end_incbin_map:
EndBlock5000:

