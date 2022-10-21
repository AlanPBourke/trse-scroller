
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
	; LineNumber: 280
	jmp block1
	; LineNumber: 28
current_screen_pointer	= $02
	; LineNumber: 29
screen_base_ptr	= $04
	; LineNumber: 30
backbuffer_base_ptr	= $08
	; LineNumber: 31
colour_base_ptr	= $16
	; LineNumber: 33
offset	dc.w	0
	; LineNumber: 35
current_screen	dc.b	$00
	; LineNumber: 35
i	dc.b	$00
	; LineNumber: 36
row	dc.b	$00
	; LineNumber: 36
col	dc.b	$00
	; LineNumber: 36
numlines	dc.b	$00
	; LineNumber: 36
this_colour	dc.b	$00
	; LineNumber: 36
this_char	dc.b	$00
	; LineNumber: 36
startline	dc.b	$00
	; LineNumber: 38
scroll	dc.b	$07
	; LineNumber: 40
caddr	dc.w	 
	org caddr+50
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
	
; // Line 65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_line_65
	;    Procedure type : User-defined procedure
	; LineNumber: 84
irq_line_65
	; LineNumber: 86
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 88
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 90
	
; //copy_colors(0); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock8
irq_line_65_ConditionalTrueBlock6: ;Main true block ;keep 
	; LineNumber: 91
	; LineNumber: 93
irq_line_65_elsedoneblock8
	; LineNumber: 95
	; RasterIRQ : Hook a procedure
	lda #$fb
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 97
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 99
	rti
end_procedure_irq_line_65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 102
irq_begin_vblank
	; LineNumber: 104
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 107
	
; //waitforraster(begin_vblank_line);				
; // Too quick ATM
	; Test Inc dec D
	dec scroll
	; LineNumber: 109
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock14
irq_begin_vblank_ConditionalTrueBlock13: ;Main true block ;keep 
	; LineNumber: 109
	; LineNumber: 112
	
; //	addbreakpoint();
	jsr swap_screens
	; LineNumber: 114
	jmp irq_begin_vblank_elsedoneblock15
irq_begin_vblank_elseblock14
	; LineNumber: 115
	; LineNumber: 117
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 120
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock35
irq_begin_vblank_ConditionalTrueBlock33: ;Main true block ;keep 
	; LineNumber: 121
	; LineNumber: 122
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 123
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 124
	jsr copy_and_shift
	; LineNumber: 125
irq_begin_vblank_elsedoneblock35
	; LineNumber: 128
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock41
irq_begin_vblank_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 129
	; LineNumber: 130
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 131
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 132
	jsr copy_and_shift
	; LineNumber: 134
irq_begin_vblank_elsedoneblock41
	; LineNumber: 135
irq_begin_vblank_elsedoneblock15
	; LineNumber: 137
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 138
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 141
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 143
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 146
copy_and_shift
	; LineNumber: 148
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 149
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 151
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 152
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
	; LineNumber: 153
	; LineNumber: 155
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
	; LineNumber: 157
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
	; LineNumber: 158
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
	; LineNumber: 159
	lda #$1
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 160
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 161
copy_and_shift_while94
copy_and_shift_loopstart98
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs copy_and_shift_elsedoneblock97
copy_and_shift_ConditionalTrueBlock95: ;Main true block ;keep 
	; LineNumber: 162
	; LineNumber: 163
	; 8 bit binop
	; Add/sub where right value is constant number
	lda col
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 164
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock112
copy_and_shift_ConditionalTrueBlock111: ;Main true block ;keep 
	; LineNumber: 165
	; LineNumber: 166
	; Load pointer array
	ldy col
	lda (screen_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (backbuffer_base_ptr),y
	; LineNumber: 168
	jmp copy_and_shift_elsedoneblock113
copy_and_shift_elseblock112
	; LineNumber: 169
	; LineNumber: 170
	; Load pointer array
	ldy col
	lda (backbuffer_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 171
copy_and_shift_elsedoneblock113
	; LineNumber: 172
	; Test Inc dec D
	inc col
	; LineNumber: 173
	jmp copy_and_shift_while94
copy_and_shift_elsedoneblock97
copy_and_shift_loopend99
	; LineNumber: 175
	; Test Inc dec D
	inc row
	; LineNumber: 177
	jmp copy_and_shift_while45
copy_and_shift_elsedoneblock48
copy_and_shift_loopend50
	; LineNumber: 179
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToBack
	;    Procedure type : User-defined procedure
	; LineNumber: 183
DrawColumn39ToBack
	; LineNumber: 185
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne DrawColumn39ToBack_elseblock121
DrawColumn39ToBack_ConditionalTrueBlock120: ;Main true block ;keep 
	; LineNumber: 185
	; LineNumber: 187
	lda #$00
	ldx #$34
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 189
	jmp DrawColumn39ToBack_elsedoneblock122
DrawColumn39ToBack_elseblock121
	; LineNumber: 190
	; LineNumber: 191
	lda #$00
	ldx #$30
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 192
DrawColumn39ToBack_elsedoneblock122
	; LineNumber: 194
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 195
	jsr Random
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 196
DrawColumn39ToBack_while127
DrawColumn39ToBack_loopstart131
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_ConditionalTrueBlock128: ;Main true block ;keep 
	; LineNumber: 197
	; LineNumber: 199
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (current_screen_pointer),y
	; LineNumber: 200
	lda current_screen_pointer
	clc
	adc #$28
	sta current_screen_pointer+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39ToBack_WordAdd136
	inc current_screen_pointer+1
DrawColumn39ToBack_WordAdd136
	; LineNumber: 202
	; Test Inc dec D
	inc i
	; LineNumber: 203
	; Test Inc dec D
	inc this_char
	; LineNumber: 205
	jmp DrawColumn39ToBack_while127
DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_loopend132
	; LineNumber: 206
	rts
end_procedure_DrawColumn39ToBack
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 210
swap_screens
	; LineNumber: 213
	
; //addbreakpoint();
	jsr DrawColumn39ToBack
	; LineNumber: 214
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 215
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 219
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne swap_screens_elseblock140
swap_screens_ConditionalTrueBlock139: ;Main true block ;keep 
	; LineNumber: 220
	; LineNumber: 221
	
; // todo this properly just invert
	lda #$1
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 222
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	; LineNumber: 225
	jmp swap_screens_elsedoneblock141
swap_screens_elseblock140
	; LineNumber: 226
	; LineNumber: 227
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 228
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 229
swap_screens_elsedoneblock141
	; LineNumber: 238
	rts
end_procedure_swap_screens
	
; //screen_bg_col := current_screen;
; //SetRegularColorMode();
; //addbreakpoint();
; //copy_colors(1); 
; // color_shift_lower
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar_slow
	;    Procedure type : User-defined procedure
	; LineNumber: 241
fillwithchar_slow
	; LineNumber: 243
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 244
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 246
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 247
	lda #$0
	; Calling storevariable on generic assign expression
	sta this_colour
	; LineNumber: 249
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 251
fillwithchar_slow_while147
fillwithchar_slow_loopstart151
	; Binary clause Simplified: LESS
	lda row
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs fillwithchar_slow_localfailed177
	jmp fillwithchar_slow_ConditionalTrueBlock148
fillwithchar_slow_localfailed177
	jmp fillwithchar_slow_elsedoneblock150
fillwithchar_slow_ConditionalTrueBlock148: ;Main true block ;keep 
	; LineNumber: 252
	; LineNumber: 254
	lda #$0
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 255
fillwithchar_slow_while179
fillwithchar_slow_loopstart183
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs fillwithchar_slow_elsedoneblock182
fillwithchar_slow_ConditionalTrueBlock180: ;Main true block ;keep 
	; LineNumber: 256
	; LineNumber: 257
	lda this_colour
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy col ; optimized, look out for bugs
	sta (colour_base_ptr),y
	; LineNumber: 258
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (screen_base_ptr),y
	; LineNumber: 259
	lda #$20
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (backbuffer_base_ptr),y
	; LineNumber: 260
	; Test Inc dec D
	inc col
	; LineNumber: 261
	jmp fillwithchar_slow_while179
fillwithchar_slow_elsedoneblock182
fillwithchar_slow_loopend184
	; LineNumber: 263
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd187
	inc screen_base_ptr+1
fillwithchar_slow_WordAdd187
	; LineNumber: 264
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd188
	inc backbuffer_base_ptr+1
fillwithchar_slow_WordAdd188
	; LineNumber: 265
	lda colour_base_ptr
	clc
	adc #$28
	sta colour_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd189
	inc colour_base_ptr+1
fillwithchar_slow_WordAdd189
	; LineNumber: 267
	; Test Inc dec D
	inc this_colour
	; LineNumber: 268
	; Test Inc dec D
	inc this_char
	; LineNumber: 269
	; Test Inc dec D
	inc row
	; LineNumber: 271
	; Binary clause Simplified: EQUALS
	lda this_char
	; Compare with pure num / var optimization
	cmp #$b;keep
	bne fillwithchar_slow_elsedoneblock193
fillwithchar_slow_ConditionalTrueBlock191: ;Main true block ;keep 
	; LineNumber: 270
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
fillwithchar_slow_elsedoneblock193
	; LineNumber: 272
	; Binary clause Simplified: EQUALS
	lda this_colour
	; Compare with pure num / var optimization
	cmp #$10;keep
	bne fillwithchar_slow_elsedoneblock199
fillwithchar_slow_ConditionalTrueBlock197: ;Main true block ;keep 
	; LineNumber: 271
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_colour
fillwithchar_slow_elsedoneblock199
	; LineNumber: 274
	jmp fillwithchar_slow_while147
fillwithchar_slow_elsedoneblock150
fillwithchar_slow_loopend152
	; LineNumber: 276
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 278
	rts
end_procedure_fillwithchar_slow
block1
main_block_begin_
	; LineNumber: 280
	sei
	; LineNumber: 281
	
; // System IRQs, not mine.
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop202
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow203
	iny
MainProgram_dtnooverflow203
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop202
	; LineNumber: 284
	
; // $D800 color address, 40 characters per column, 25 rows
; //DefineScreen();
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 285
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 286
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 287
	lda $d018
	and #%11110001
	ora #4
	sta $d018
	; LineNumber: 288
	; Poke
	; Optimization: shift is zero
	lda #$17
	sta $d018
	; LineNumber: 290
	
; // Lower\upper chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 291
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 292
	jsr fillwithchar_slow
	; LineNumber: 295
	
; //waitforspace();
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
	; LineNumber: 296
	jmp * ; loop like (�/%
	; LineNumber: 298
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

