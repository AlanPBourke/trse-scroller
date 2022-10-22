
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
	; LineNumber: 304
	jmp block1
	; LineNumber: 32
	; LineNumber: 33
	; LineNumber: 36
current_screen_ptr	= $02
	; LineNumber: 36
screen_base_ptr	= $04
	; LineNumber: 36
backbuffer_base_ptr	= $08
	; LineNumber: 36
map_ptr	= $16
	; LineNumber: 38
offset	dc.w	$00
	; LineNumber: 38
map_column	dc.w	$00
	; LineNumber: 40
current_screen	dc.b	$00
	; LineNumber: 40
i	dc.b	$00
	; LineNumber: 41
row	dc.b	$00
	; LineNumber: 41
col	dc.b	$00
	; LineNumber: 41
numlines	dc.b	$00
	; LineNumber: 41
startline	dc.b	$00
	; LineNumber: 43
scroll	dc.b	$07
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
	; ***********  Defining procedure : initrandom256
	;    Procedure type : Built-in function
	;    Requires initialization : no
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip3
	eor #$4d
initrandom256_RandomSkip3
	eor $dc04
	sta Random+1
	rts
end_procedure_initrandom256
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
	; LineNumber: 61
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 63
	
; //startline := 0;
; //numlines := 12;
; //copy_colors(); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock8
irq_line_65_ConditionalTrueBlock6: ;Main true block ;keep 
	; LineNumber: 64
	; LineNumber: 68
irq_line_65_elsedoneblock8
	; LineNumber: 70
	; RasterIRQ : Hook a procedure
	lda #$fb
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
	; LineNumber: 104
	
; //waitforraster(begin_vblank_line);				
; // Too quick ATM
	; Test Inc dec D
	dec scroll
	; LineNumber: 106
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock14
irq_begin_vblank_ConditionalTrueBlock13: ;Main true block ;keep 
	; LineNumber: 106
	; LineNumber: 109
	
; //	addbreakpoint();
	jsr swap_screens
	; LineNumber: 111
	jmp irq_begin_vblank_elsedoneblock15
irq_begin_vblank_elseblock14
	; LineNumber: 112
	; LineNumber: 114
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 117
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock35
irq_begin_vblank_ConditionalTrueBlock33: ;Main true block ;keep 
	; LineNumber: 118
	; LineNumber: 119
	
; // Copy top half of char screen to back buffer.
	lda #$4
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 120
	
; // zero-based
	lda #$8
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 121
	
; // one based
	jsr copy_and_shift
	; LineNumber: 122
irq_begin_vblank_elsedoneblock35
	; LineNumber: 125
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock41
irq_begin_vblank_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 126
	; LineNumber: 127
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 128
	lda #$9
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 129
	jsr copy_and_shift
	; LineNumber: 131
irq_begin_vblank_elsedoneblock41
	; LineNumber: 132
irq_begin_vblank_elsedoneblock15
	; LineNumber: 135
	
; //hideborderx(1);
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 136
	
; //addbreakpoint();
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 139
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 141
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 144
copy_and_shift
	; LineNumber: 146
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 147
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 149
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 150
copy_and_shift_while45
copy_and_shift_loopstart49
	; Binary clause Simplified: LESS
	lda row
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs copy_and_shift_localfailed84
	jmp copy_and_shift_ConditionalTrueBlock46
copy_and_shift_localfailed84
	jmp copy_and_shift_elsedoneblock48
copy_and_shift_ConditionalTrueBlock46: ;Main true block ;keep 
	; LineNumber: 151
	; LineNumber: 153
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda startline
	clc
	adc row
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc copy_and_shift_skip87
	iny
copy_and_shift_skip87
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta offset
	sty offset+1
	; LineNumber: 155
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy offset+1 ; keep
copy_and_shift_rightvarInteger_var90 = $54
	sta copy_and_shift_rightvarInteger_var90
	sty copy_and_shift_rightvarInteger_var90+1
	; Integer constant assigning
	ldy #$30
	lda #$00
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var90
copy_and_shift_wordAdd88
	sta copy_and_shift_rightvarInteger_var90
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var90+1
	tay
	lda copy_and_shift_rightvarInteger_var90
	sta screen_base_ptr
	sty screen_base_ptr+1
	; LineNumber: 156
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy offset+1 ; keep
	lda offset
copy_and_shift_rightvarInteger_var93 = $54
	sta copy_and_shift_rightvarInteger_var93
	sty copy_and_shift_rightvarInteger_var93+1
	; Integer constant assigning
	ldy #$34
	lda #$00
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var93
copy_and_shift_wordAdd91
	sta copy_and_shift_rightvarInteger_var93
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var93+1
	tay
	lda copy_and_shift_rightvarInteger_var93
	sta backbuffer_base_ptr
	sty backbuffer_base_ptr+1
	; LineNumber: 157
	lda #$1
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 158
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 159
copy_and_shift_while94
copy_and_shift_loopstart98
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs copy_and_shift_elsedoneblock97
copy_and_shift_ConditionalTrueBlock95: ;Main true block ;keep 
	; LineNumber: 160
	; LineNumber: 161
	; 8 bit binop
	; Add/sub where right value is constant number
	lda col
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 162
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock112
copy_and_shift_ConditionalTrueBlock111: ;Main true block ;keep 
	; LineNumber: 163
	; LineNumber: 164
	; Load pointer array
	ldy col
	lda (screen_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (backbuffer_base_ptr),y
	; LineNumber: 166
	jmp copy_and_shift_elsedoneblock113
copy_and_shift_elseblock112
	; LineNumber: 167
	; LineNumber: 168
	; Load pointer array
	ldy col
	lda (backbuffer_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 169
copy_and_shift_elsedoneblock113
	; LineNumber: 170
	; Test Inc dec D
	inc col
	; LineNumber: 171
	jmp copy_and_shift_while94
copy_and_shift_elsedoneblock97
copy_and_shift_loopend99
	; LineNumber: 173
	; Test Inc dec D
	inc row
	; LineNumber: 175
	jmp copy_and_shift_while45
copy_and_shift_elsedoneblock48
copy_and_shift_loopend50
	; LineNumber: 177
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39FromMap
	;    Procedure type : User-defined procedure
	; LineNumber: 208
DrawColumn39FromMap
	; LineNumber: 210
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne DrawColumn39FromMap_elseblock121
DrawColumn39FromMap_ConditionalTrueBlock120: ;Main true block ;keep 
	; LineNumber: 210
	; LineNumber: 212
	lda #$00
	ldx #$34
	sta current_screen_ptr
	stx current_screen_ptr+1
	; LineNumber: 214
	jmp DrawColumn39FromMap_elsedoneblock122
DrawColumn39FromMap_elseblock121
	; LineNumber: 215
	; LineNumber: 216
	lda #$00
	ldx #$30
	sta current_screen_ptr
	stx current_screen_ptr+1
	; LineNumber: 217
DrawColumn39FromMap_elsedoneblock122
	; LineNumber: 221
	
; // 4 blank rows, then 17 map rows, then 4 blank rows
; // Base + column offset +(row offset * 512)
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 223
	lda current_screen_ptr
	clc
	adc #$a0
	sta current_screen_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd127
	inc current_screen_ptr+1
DrawColumn39FromMap_WordAdd127
	; LineNumber: 224
	
; // Start on screen row 4
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy map_column+1 ; keep
	lda map_column
DrawColumn39FromMap_rightvarInteger_var130 = $54
	sta DrawColumn39FromMap_rightvarInteger_var130
	sty DrawColumn39FromMap_rightvarInteger_var130+1
	; Integer constant assigning
	ldy #$50
	lda #$00
	; Low bit binop:
	clc
	adc DrawColumn39FromMap_rightvarInteger_var130
DrawColumn39FromMap_wordAdd128
	sta DrawColumn39FromMap_rightvarInteger_var130
	; High-bit binop
	tya
	adc DrawColumn39FromMap_rightvarInteger_var130+1
	tay
	lda DrawColumn39FromMap_rightvarInteger_var130
	sta map_ptr
	sty map_ptr+1
	; LineNumber: 235
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
DrawColumn39FromMap_forloop131
	; LineNumber: 227
	; LineNumber: 229
	; Load pointer array
	ldy #$0
	lda (map_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (current_screen_ptr),y
	; LineNumber: 230
	lda current_screen_ptr
	clc
	adc #$28
	sta current_screen_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd140
	inc current_screen_ptr+1
DrawColumn39FromMap_WordAdd140
	; LineNumber: 231
	lda map_ptr
	clc
	adc #$00
	sta map_ptr+0
	lda map_ptr+1
	adc #$02
	sta map_ptr+1
	; LineNumber: 234
DrawColumn39FromMap_forloopcounter133
DrawColumn39FromMap_loopstart134
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$11
	cmp i ;keep
	bcs DrawColumn39FromMap_forloop131
DrawColumn39FromMap_loopdone142: ;keep
DrawColumn39FromMap_forloopend132
DrawColumn39FromMap_loopend135
	; LineNumber: 236
	lda map_column
	clc
	adc #$01
	sta map_column+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39FromMap_WordAdd143
	inc map_column+1
DrawColumn39FromMap_WordAdd143
	; LineNumber: 237
	; Binary clause INTEGER: EQUALS
	; Compare INTEGER with pure num / var optimization. GREATER. 
	lda map_column+1   ; compare high bytes
	cmp #$00 ;keep
	bne DrawColumn39FromMap_elsedoneblock147
	lda map_column
	cmp #$fe ;keep
	bne DrawColumn39FromMap_elsedoneblock147
	jmp DrawColumn39FromMap_ConditionalTrueBlock145
DrawColumn39FromMap_ConditionalTrueBlock145: ;Main true block ;keep 
	; LineNumber: 236
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta map_column
	sty map_column+1
DrawColumn39FromMap_elsedoneblock147
	; LineNumber: 239
	rts
end_procedure_DrawColumn39FromMap
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 242
swap_screens
	; LineNumber: 245
	
; //addbreakpoint();
	jsr DrawColumn39FromMap
	; LineNumber: 246
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 247
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 250
	
; // todo this properly just invert
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
	; LineNumber: 251
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne swap_screens_elseblock153
swap_screens_ConditionalTrueBlock152: ;Main true block ;keep 
	; LineNumber: 250
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	jmp swap_screens_elsedoneblock154
swap_screens_elseblock153
	; LineNumber: 250
	lda $d018
	and #%00001111
	ora #192
	sta $d018
swap_screens_elsedoneblock154
	; LineNumber: 253
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 259
	rts
end_procedure_swap_screens
block1
main_block_begin_
	; LineNumber: 304
	sei
	; LineNumber: 308
	
; // System IRQs, not mine.
; //	CreateAddressTable( #caddr, $D800, 40, 25 );   
; // $D800 color address, 40 characters per column, 25 rows
; //DefineScreen();
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 309
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 310
	; Clear screen with offset
	lda #$1
	ldx #$fa
MainProgram_clearloop159
	dex
	sta $0000+$3000,x
	sta $00fa+$3000,x
	sta $01f4+$3000,x
	sta $02ee+$3000,x
	bne MainProgram_clearloop159
	; LineNumber: 310
	; Clear screen with offset
	lda #$1
	ldx #$fa
MainProgram_clearloop160
	dex
	sta $0000+$3400,x
	sta $00fa+$3400,x
	sta $01f4+$3400,x
	sta $02ee+$3400,x
	bne MainProgram_clearloop160
	; LineNumber: 312
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 313
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d020
	; LineNumber: 314
	; Assigning memory location
	lda #$1
	; Calling storevariable on generic assign expression
	sta $d021
	; LineNumber: 315
	lda #$7
	; Calling storevariable on generic assign expression
	sta $D021+$1
	; LineNumber: 316
	lda #$8
	; Calling storevariable on generic assign expression
	sta $D021+$1
	; LineNumber: 321
	
; //	screen_bg_col:=black;
; //	screen_fg_col:=0;
; //	screen_fg_col[2]:=dark_grey;
; //screen_fg_col[1]:=grey;
	; Poke
	; Optimization: shift is zero
	lda #$17
	sta $d018
	; LineNumber: 323
	
; // Lower\upper chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 324
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 327
	
; //fillwithchar_slow();
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 329
	
; // Do this last.
	lda $d018
	and #%11110001
	ora #8
	sta $d018
	; LineNumber: 330
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
	; LineNumber: 331
	jmp * ; loop like (�/%
	; LineNumber: 333
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

