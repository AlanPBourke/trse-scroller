
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
	; LineNumber: 269
	jmp block1
	; LineNumber: 28
current_screen_pointer	= $02
	; LineNumber: 28
screen_base_ptr	= $04
	; LineNumber: 28
backbuffer_base_ptr	= $08
	; LineNumber: 28
colour_base_ptr	= $16
	; LineNumber: 30
offset	dc.w	0
	; LineNumber: 32
current_screen	dc.b	$00
	; LineNumber: 32
i	dc.b	$00
	; LineNumber: 33
row	dc.b	$00
	; LineNumber: 33
col	dc.b	$00
	; LineNumber: 33
numlines	dc.b	$00
	; LineNumber: 33
this_colour	dc.b	$00
	; LineNumber: 33
this_char	dc.b	$00
	; LineNumber: 33
startline	dc.b	$00
	; LineNumber: 35
scroll	dc.b	$07
	; LineNumber: 37
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_line_65
	;    Procedure type : User-defined procedure
	; LineNumber: 49
irq_line_65
	; LineNumber: 51
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 53
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 55
	
; //copy_colors(); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock8
irq_line_65_ConditionalTrueBlock6: ;Main true block ;keep 
	; LineNumber: 56
	; LineNumber: 58
irq_line_65_elsedoneblock8
	; LineNumber: 60
	; RasterIRQ : Hook a procedure
	lda #$fb
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 61
	; Regular text mode 
	lda $D011
	and #%01011111
	sta $D011
	; LineNumber: 62
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 64
	rti
end_procedure_irq_line_65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 89
irq_begin_vblank
	; LineNumber: 91
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 94
	
; //waitforraster(begin_vblank_line);				
; // Too quick ATM
	; Test Inc dec D
	dec scroll
	; LineNumber: 96
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock14
irq_begin_vblank_ConditionalTrueBlock13: ;Main true block ;keep 
	; LineNumber: 96
	; LineNumber: 99
	
; //	addbreakpoint();
	jsr swap_screens
	; LineNumber: 101
	jmp irq_begin_vblank_elsedoneblock15
irq_begin_vblank_elseblock14
	; LineNumber: 102
	; LineNumber: 104
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 107
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock35
irq_begin_vblank_ConditionalTrueBlock33: ;Main true block ;keep 
	; LineNumber: 108
	; LineNumber: 109
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 110
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 111
	jsr copy_and_shift
	; LineNumber: 112
irq_begin_vblank_elsedoneblock35
	; LineNumber: 115
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock41
irq_begin_vblank_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 116
	; LineNumber: 117
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 118
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 119
	jsr copy_and_shift
	; LineNumber: 121
irq_begin_vblank_elsedoneblock41
	; LineNumber: 122
irq_begin_vblank_elsedoneblock15
	; LineNumber: 125
	
; //hideborderx(1);
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 125
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 128
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 130
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 133
copy_and_shift
	; LineNumber: 135
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 136
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 138
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 139
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
	; LineNumber: 140
	; LineNumber: 142
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
	; LineNumber: 144
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
	; LineNumber: 145
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
	; LineNumber: 146
	lda #$1
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 147
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 148
copy_and_shift_while94
copy_and_shift_loopstart98
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs copy_and_shift_elsedoneblock97
copy_and_shift_ConditionalTrueBlock95: ;Main true block ;keep 
	; LineNumber: 149
	; LineNumber: 150
	; 8 bit binop
	; Add/sub where right value is constant number
	lda col
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 151
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock112
copy_and_shift_ConditionalTrueBlock111: ;Main true block ;keep 
	; LineNumber: 152
	; LineNumber: 153
	; Load pointer array
	ldy col
	lda (screen_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (backbuffer_base_ptr),y
	; LineNumber: 155
	jmp copy_and_shift_elsedoneblock113
copy_and_shift_elseblock112
	; LineNumber: 156
	; LineNumber: 157
	; Load pointer array
	ldy col
	lda (backbuffer_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 158
copy_and_shift_elsedoneblock113
	; LineNumber: 159
	; Test Inc dec D
	inc col
	; LineNumber: 160
	jmp copy_and_shift_while94
copy_and_shift_elsedoneblock97
copy_and_shift_loopend99
	; LineNumber: 162
	; Test Inc dec D
	inc row
	; LineNumber: 164
	jmp copy_and_shift_while45
copy_and_shift_elsedoneblock48
copy_and_shift_loopend50
	; LineNumber: 166
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToBack
	;    Procedure type : User-defined procedure
	; LineNumber: 170
DrawColumn39ToBack
	; LineNumber: 172
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne DrawColumn39ToBack_elseblock121
DrawColumn39ToBack_ConditionalTrueBlock120: ;Main true block ;keep 
	; LineNumber: 172
	; LineNumber: 174
	lda #$00
	ldx #$34
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 176
	jmp DrawColumn39ToBack_elsedoneblock122
DrawColumn39ToBack_elseblock121
	; LineNumber: 177
	; LineNumber: 178
	lda #$00
	ldx #$30
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 179
DrawColumn39ToBack_elsedoneblock122
	; LineNumber: 181
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 182
	jsr Random
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 183
DrawColumn39ToBack_while127
DrawColumn39ToBack_loopstart131
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_ConditionalTrueBlock128: ;Main true block ;keep 
	; LineNumber: 184
	; LineNumber: 186
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (current_screen_pointer),y
	; LineNumber: 187
	lda current_screen_pointer
	clc
	adc #$28
	sta current_screen_pointer+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39ToBack_WordAdd136
	inc current_screen_pointer+1
DrawColumn39ToBack_WordAdd136
	; LineNumber: 189
	; Test Inc dec D
	inc i
	; LineNumber: 190
	; Test Inc dec D
	inc this_char
	; LineNumber: 192
	jmp DrawColumn39ToBack_while127
DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_loopend132
	; LineNumber: 193
	rts
end_procedure_DrawColumn39ToBack
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 197
swap_screens
	; LineNumber: 200
	
; //addbreakpoint();
	jsr DrawColumn39ToBack
	; LineNumber: 201
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 202
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 205
	
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
	; LineNumber: 224
	rts
end_procedure_swap_screens
	
; //	if current_screen = 0 then 
; //	begin
; //		current_screen := 1;
; //		SetScreenLocation(screen_backbuffer_base);
; //	end
; //	else
; //	begin
; //		current_screen := 0;
; //		SetScreenLocation(screen_base);
; //	end;
; //SetRegularColorMode();
; //addbreakpoint();
; //copy_colors(1); 
; // color_shift_lower
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar_slow
	;    Procedure type : User-defined procedure
	; LineNumber: 227
fillwithchar_slow
	; LineNumber: 229
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 230
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 231
	ldx #$d8
	sta colour_base_ptr
	stx colour_base_ptr+1
	; LineNumber: 233
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 234
	lda #$0
	; Calling storevariable on generic assign expression
	sta this_colour
	; LineNumber: 236
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 238
fillwithchar_slow_while139
fillwithchar_slow_loopstart143
	; Binary clause Simplified: LESS
	lda row
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs fillwithchar_slow_localfailed175
	jmp fillwithchar_slow_ConditionalTrueBlock140
fillwithchar_slow_localfailed175
	jmp fillwithchar_slow_elsedoneblock142
fillwithchar_slow_ConditionalTrueBlock140: ;Main true block ;keep 
	; LineNumber: 239
	; LineNumber: 241
	lda #$0
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 242
	; Calling storevariable on generic assign expression
	sta this_colour
	; LineNumber: 243
fillwithchar_slow_while177
fillwithchar_slow_loopstart181
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs fillwithchar_slow_elsedoneblock180
fillwithchar_slow_ConditionalTrueBlock178: ;Main true block ;keep 
	; LineNumber: 244
	; LineNumber: 246
	
; //colour_base_ptr[col] := this_colour;
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy col ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 247
	lda #$20
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (backbuffer_base_ptr),y
	; LineNumber: 248
	; Test Inc dec D
	inc col
	; LineNumber: 249
	; Test Inc dec D
	inc this_colour
	; LineNumber: 250
	; Binary clause Simplified: EQUALS
	lda this_colour
	; Compare with pure num / var optimization
	cmp #$f;keep
	bne fillwithchar_slow_elsedoneblock194
fillwithchar_slow_ConditionalTrueBlock192: ;Main true block ;keep 
	; LineNumber: 249
	lda #$0
	; Calling storevariable on generic assign expression
	sta this_colour
fillwithchar_slow_elsedoneblock194
	; LineNumber: 251
	jmp fillwithchar_slow_while177
fillwithchar_slow_elsedoneblock180
fillwithchar_slow_loopend182
	; LineNumber: 253
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd197
	inc screen_base_ptr+1
fillwithchar_slow_WordAdd197
	; LineNumber: 254
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd198
	inc backbuffer_base_ptr+1
fillwithchar_slow_WordAdd198
	; LineNumber: 255
	lda colour_base_ptr
	clc
	adc #$28
	sta colour_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd199
	inc colour_base_ptr+1
fillwithchar_slow_WordAdd199
	; LineNumber: 258
	; Test Inc dec D
	inc this_char
	; LineNumber: 259
	; Test Inc dec D
	inc row
	; LineNumber: 261
	; Binary clause Simplified: EQUALS
	lda this_char
	; Compare with pure num / var optimization
	cmp #$b;keep
	bne fillwithchar_slow_elsedoneblock203
fillwithchar_slow_ConditionalTrueBlock201: ;Main true block ;keep 
	; LineNumber: 260
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
fillwithchar_slow_elsedoneblock203
	; LineNumber: 263
	jmp fillwithchar_slow_while139
fillwithchar_slow_elsedoneblock142
fillwithchar_slow_loopend144
	; LineNumber: 265
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 267
	rts
end_procedure_fillwithchar_slow
block1
main_block_begin_
	; LineNumber: 269
	sei
	; LineNumber: 270
	
; // System IRQs, not mine.
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop206
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow207
	iny
MainProgram_dtnooverflow207
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop206
	; LineNumber: 273
	
; // $D800 color address, 40 characters per column, 25 rows
; //DefineScreen();
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 274
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 275
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 276
	lda $d018
	and #%11110001
	ora #4
	sta $d018
	; LineNumber: 277
	; Poke
	; Optimization: shift is zero
	lda #$17
	sta $d018
	; LineNumber: 279
	
; // Lower\upper chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 280
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 281
	jsr fillwithchar_slow
	; LineNumber: 284
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
	; LineNumber: 285
	jmp * ; loop like (�/%
	; LineNumber: 287
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

