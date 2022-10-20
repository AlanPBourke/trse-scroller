
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
SplitScroller2
	; LineNumber: 280
	jmp block1
	; LineNumber: 29
current_screen_pointer	= $02
	; LineNumber: 30
screen_base_ptr	= $04
	; LineNumber: 31
backbuffer_base_ptr	= $08
	; LineNumber: 33
row	dc.b	$00
	; LineNumber: 33
col	dc.b	$00
	; LineNumber: 33
current_screen	dc.b	$00
	; LineNumber: 33
i	dc.b	$00
	; LineNumber: 36
startline	dc.b	$00
	; LineNumber: 37
numlines	dc.b	$00
	; LineNumber: 39
offset	dc.w	0
	; LineNumber: 40
this_char	dc.b	$01
	; LineNumber: 41
scroll	dc.b	$07
	; LineNumber: 43
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
	; LineNumber: 87
irq_line_65
	; LineNumber: 89
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 91
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 93
	
; //copy_colors(0); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock8
irq_line_65_ConditionalTrueBlock6: ;Main true block ;keep 
	; LineNumber: 94
	; LineNumber: 96
irq_line_65_elsedoneblock8
	; LineNumber: 98
	; RasterIRQ : Hook a procedure
	lda #$fb
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 100
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 102
	rti
end_procedure_irq_line_65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 105
irq_begin_vblank
	; LineNumber: 107
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 107
	; wait for raster
	ldx #$fb ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 110
	
; // Too quick ATM
	; Test Inc dec D
	dec scroll
	; LineNumber: 112
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock14
irq_begin_vblank_ConditionalTrueBlock13: ;Main true block ;keep 
	; LineNumber: 112
	; LineNumber: 115
	
; //	addbreakpoint();
	jsr swap_screens
	; LineNumber: 117
	jmp irq_begin_vblank_elsedoneblock15
irq_begin_vblank_elseblock14
	; LineNumber: 118
	; LineNumber: 120
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 123
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock35
irq_begin_vblank_ConditionalTrueBlock33: ;Main true block ;keep 
	; LineNumber: 124
	; LineNumber: 125
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 126
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 127
	jsr copy_and_shift
	; LineNumber: 128
irq_begin_vblank_elsedoneblock35
	; LineNumber: 131
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock41
irq_begin_vblank_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 132
	; LineNumber: 133
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 134
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 135
	jsr copy_and_shift
	; LineNumber: 137
irq_begin_vblank_elsedoneblock41
	; LineNumber: 138
irq_begin_vblank_elsedoneblock15
	; LineNumber: 141
	
; //hideborderx(1);
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 144
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 146
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 149
copy_and_shift
	; LineNumber: 151
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 152
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 154
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 155
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
	; LineNumber: 156
	; LineNumber: 158
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
	; LineNumber: 162
	
; //addbreakpoint();		
; //screen_base_ptr := screen_base_ptr + offset;
; //backbuffer_base_ptr :=  backbuffer_base_ptr + offset;
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
	; LineNumber: 163
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
	; LineNumber: 164
	lda #$1
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 165
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 166
copy_and_shift_while94
copy_and_shift_loopstart98
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs copy_and_shift_elsedoneblock97
copy_and_shift_ConditionalTrueBlock95: ;Main true block ;keep 
	; LineNumber: 167
	; LineNumber: 168
	; 8 bit binop
	; Add/sub where right value is constant number
	lda col
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 169
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock112
copy_and_shift_ConditionalTrueBlock111: ;Main true block ;keep 
	; LineNumber: 170
	; LineNumber: 171
	; Load pointer array
	ldy col
	lda (screen_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (backbuffer_base_ptr),y
	; LineNumber: 173
	jmp copy_and_shift_elsedoneblock113
copy_and_shift_elseblock112
	; LineNumber: 174
	; LineNumber: 175
	; Load pointer array
	ldy col
	lda (backbuffer_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 176
copy_and_shift_elsedoneblock113
	; LineNumber: 177
	; Test Inc dec D
	inc col
	; LineNumber: 178
	jmp copy_and_shift_while94
copy_and_shift_elsedoneblock97
copy_and_shift_loopend99
	; LineNumber: 180
	; Test Inc dec D
	inc row
	; LineNumber: 182
	jmp copy_and_shift_while45
copy_and_shift_elsedoneblock48
copy_and_shift_loopend50
	; LineNumber: 184
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToBack
	;    Procedure type : User-defined procedure
	; LineNumber: 188
DrawColumn39ToBack
	; LineNumber: 190
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne DrawColumn39ToBack_elseblock121
DrawColumn39ToBack_ConditionalTrueBlock120: ;Main true block ;keep 
	; LineNumber: 190
	; LineNumber: 192
	lda #$00
	ldx #$30
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 194
	jmp DrawColumn39ToBack_elsedoneblock122
DrawColumn39ToBack_elseblock121
	; LineNumber: 195
	; LineNumber: 196
	lda #$00
	ldx #$34
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 197
DrawColumn39ToBack_elsedoneblock122
	; LineNumber: 200
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 201
	jsr Random
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 202
DrawColumn39ToBack_while127
DrawColumn39ToBack_loopstart131
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_ConditionalTrueBlock128: ;Main true block ;keep 
	; LineNumber: 203
	; LineNumber: 205
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (current_screen_pointer),y
	; LineNumber: 206
	lda current_screen_pointer
	clc
	adc #$28
	sta current_screen_pointer+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39ToBack_WordAdd136
	inc current_screen_pointer+1
DrawColumn39ToBack_WordAdd136
	; LineNumber: 208
	; Test Inc dec D
	inc i
	; LineNumber: 209
	; Test Inc dec D
	inc this_char
	; LineNumber: 211
	jmp DrawColumn39ToBack_while127
DrawColumn39ToBack_elsedoneblock130
DrawColumn39ToBack_loopend132
	; LineNumber: 212
	rts
end_procedure_DrawColumn39ToBack
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 216
swap_screens
	; LineNumber: 218
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 219
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 223
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne swap_screens_elseblock140
swap_screens_ConditionalTrueBlock139: ;Main true block ;keep 
	; LineNumber: 224
	; LineNumber: 225
	
; // todo this properly just invert
	lda #$1
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 226
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	; LineNumber: 229
	jmp swap_screens_elsedoneblock141
swap_screens_elseblock140
	; LineNumber: 230
	; LineNumber: 231
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 232
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 233
swap_screens_elsedoneblock141
	; LineNumber: 234
	; Assigning memory location
	lda current_screen
	; Calling storevariable on generic assign expression
	sta $d020
	; LineNumber: 238
	
; //SetRegularColorMode();
	jsr DrawColumn39ToBack
	; LineNumber: 242
	rts
end_procedure_swap_screens
	
; //addbreakpoint();
; //copy_colors(1); 
; // color_shift_lower
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar_slow
	;    Procedure type : User-defined procedure
	; LineNumber: 245
fillwithchar_slow
	; LineNumber: 247
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 248
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 250
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 252
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 254
fillwithchar_slow_while147
fillwithchar_slow_loopstart151
	; Binary clause Simplified: LESS
	lda row
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs fillwithchar_slow_elsedoneblock150
fillwithchar_slow_ConditionalTrueBlock148: ;Main true block ;keep 
	; LineNumber: 255
	; LineNumber: 257
	lda #$0
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 258
fillwithchar_slow_while171
fillwithchar_slow_loopstart175
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs fillwithchar_slow_elsedoneblock174
fillwithchar_slow_ConditionalTrueBlock172: ;Main true block ;keep 
	; LineNumber: 259
	; LineNumber: 260
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy col ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 261
	lda #$20
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (backbuffer_base_ptr),y
	; LineNumber: 262
	; Test Inc dec D
	inc col
	; LineNumber: 263
	jmp fillwithchar_slow_while171
fillwithchar_slow_elsedoneblock174
fillwithchar_slow_loopend176
	; LineNumber: 265
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd179
	inc screen_base_ptr+1
fillwithchar_slow_WordAdd179
	; LineNumber: 266
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_slow_WordAdd180
	inc backbuffer_base_ptr+1
fillwithchar_slow_WordAdd180
	; LineNumber: 268
	; Test Inc dec D
	inc this_char
	; LineNumber: 270
	; Binary clause Simplified: EQUALS
	lda this_char
	; Compare with pure num / var optimization
	cmp #$b;keep
	bne fillwithchar_slow_elsedoneblock184
fillwithchar_slow_ConditionalTrueBlock182: ;Main true block ;keep 
	; LineNumber: 269
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
fillwithchar_slow_elsedoneblock184
	; LineNumber: 271
	; Test Inc dec D
	inc row
	; LineNumber: 273
	jmp fillwithchar_slow_while147
fillwithchar_slow_elsedoneblock150
fillwithchar_slow_loopend152
	; LineNumber: 275
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 277
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
MainProgram_dtloop187
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow188
	iny
MainProgram_dtnooverflow188
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop187
	; LineNumber: 283
	
; // $D800 color address, 40 characters per column, 25 rows
screenmemory =  $fe
colormemory =  $fb
	; LineNumber: 284
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
	and #%00001111
	ora #192
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

