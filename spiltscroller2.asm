
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
	; LineNumber: 262
	jmp block1
	; LineNumber: 29
current_screen_pointer	= $02
	; LineNumber: 30
screen_base_ptr	= $04
	; LineNumber: 31
backbuffer_base_ptr	= $08
	; LineNumber: 33
current_screen	dc.b	$00
	; LineNumber: 33
i	dc.b	$00
	; LineNumber: 34
number_1	dc.b	$31
	; LineNumber: 35
number_2	dc.b	$32
	; LineNumber: 36
startline	dc.b	$00
	; LineNumber: 37
numlines	dc.b	$00
	; LineNumber: 39
this_char	dc.b	$01
	; LineNumber: 40
scroll	dc.b	$07
	; LineNumber: 42
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
	; ***********  Defining procedure : screen_swap
	;    Procedure type : User-defined procedure
	rts
end_procedure_screen_swap
	
; // Line 65
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_line_65
	;    Procedure type : User-defined procedure
	; LineNumber: 86
irq_line_65
	; LineNumber: 88
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 90
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 92
	
; //copy_colors(0); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock7
irq_line_65_ConditionalTrueBlock5: ;Main true block ;keep 
	; LineNumber: 93
	; LineNumber: 95
irq_line_65_elsedoneblock7
	; LineNumber: 97
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 99
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 101
	rti
end_procedure_irq_line_65
	
; // vblank line starts at 245
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
	ldx #$f5 ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 110
	
; // Too quick ATM
; //addbreakpoint();		
	; Test Inc dec D
	dec scroll
	; LineNumber: 112
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock13
irq_begin_vblank_ConditionalTrueBlock12: ;Main true block ;keep 
	; LineNumber: 112
	; LineNumber: 114
	jsr swap_screens
	; LineNumber: 116
	jmp irq_begin_vblank_elsedoneblock14
irq_begin_vblank_elseblock13
	; LineNumber: 117
	; LineNumber: 119
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 122
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock34
irq_begin_vblank_ConditionalTrueBlock32: ;Main true block ;keep 
	; LineNumber: 123
	; LineNumber: 124
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 125
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 126
	jsr copy_and_shift
	; LineNumber: 127
irq_begin_vblank_elsedoneblock34
	; LineNumber: 130
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock40
irq_begin_vblank_ConditionalTrueBlock38: ;Main true block ;keep 
	; LineNumber: 131
	; LineNumber: 132
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 133
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 134
	jsr copy_and_shift
	; LineNumber: 136
irq_begin_vblank_elsedoneblock40
	; LineNumber: 137
irq_begin_vblank_elsedoneblock14
	; LineNumber: 140
	
; //hideborderx(1);
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 143
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 145
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 148
copy_and_shift
	; LineNumber: 150
	; Generic 16 bit op
	ldy #0
	lda #$1
copy_and_shift_rightvarInteger_var46 = $54
	sta copy_and_shift_rightvarInteger_var46
	sty copy_and_shift_rightvarInteger_var46+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$30
	lda #$00
copy_and_shift_rightvarInteger_var49 = $56
	sta copy_and_shift_rightvarInteger_var49
	sty copy_and_shift_rightvarInteger_var49+1
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
	adc copy_and_shift_rightvarInteger_var49
copy_and_shift_wordAdd47
	sta copy_and_shift_rightvarInteger_var49
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var49+1
	tay
	lda copy_and_shift_rightvarInteger_var49
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var46
copy_and_shift_wordAdd44
	sta copy_and_shift_rightvarInteger_var46
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var46+1
	tay
	lda copy_and_shift_rightvarInteger_var46
	sta screen_base_ptr
	sty screen_base_ptr+1
	; LineNumber: 151
	; Generic 16 bit op
	ldy #0
	lda #$1
copy_and_shift_rightvarInteger_var52 = $54
	sta copy_and_shift_rightvarInteger_var52
	sty copy_and_shift_rightvarInteger_var52+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$34
	lda #$00
copy_and_shift_rightvarInteger_var55 = $56
	sta copy_and_shift_rightvarInteger_var55
	sty copy_and_shift_rightvarInteger_var55+1
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
	adc copy_and_shift_rightvarInteger_var55
copy_and_shift_wordAdd53
	sta copy_and_shift_rightvarInteger_var55
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var55+1
	tay
	lda copy_and_shift_rightvarInteger_var55
	; Low bit binop:
	clc
	adc copy_and_shift_rightvarInteger_var52
copy_and_shift_wordAdd50
	sta copy_and_shift_rightvarInteger_var52
	; High-bit binop
	tya
	adc copy_and_shift_rightvarInteger_var52+1
	tay
	lda copy_and_shift_rightvarInteger_var52
	sta backbuffer_base_ptr
	sty backbuffer_base_ptr+1
	; LineNumber: 153
	lda #$14
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 154
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 155
copy_and_shift_while56
copy_and_shift_loopstart60
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs copy_and_shift_elsedoneblock59
copy_and_shift_ConditionalTrueBlock57: ;Main true block ;keep 
	; LineNumber: 156
	; LineNumber: 158
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock80
copy_and_shift_ConditionalTrueBlock79: ;Main true block ;keep 
	; LineNumber: 159
	; LineNumber: 160
	; memcpy
	ldy #0
copy_and_shift_memcpy87
	lda (screen_base_ptr),y
	sta (backbuffer_base_ptr),y
	iny
	cpy #$27
	bne copy_and_shift_memcpy87
	; LineNumber: 162
	jmp copy_and_shift_elsedoneblock81
copy_and_shift_elseblock80
	; LineNumber: 163
	; LineNumber: 164
	; memcpy
	ldy #0
copy_and_shift_memcpy89
	lda (backbuffer_base_ptr),y
	sta (screen_base_ptr),y
	iny
	cpy #$27
	bne copy_and_shift_memcpy89
	; LineNumber: 165
copy_and_shift_elsedoneblock81
	; LineNumber: 167
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd90
	inc screen_base_ptr+1
copy_and_shift_WordAdd90
	; LineNumber: 168
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd91
	inc backbuffer_base_ptr+1
copy_and_shift_WordAdd91
	; LineNumber: 170
	; Test Inc dec D
	inc i
	; LineNumber: 172
	jmp copy_and_shift_while56
copy_and_shift_elsedoneblock59
copy_and_shift_loopend61
	; LineNumber: 173
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToBack
	;    Procedure type : User-defined procedure
	; LineNumber: 197
DrawColumn39ToBack
	; LineNumber: 200
	
; // on the back buffer.
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$34
	lda #$00
DrawColumn39ToBack_rightvarInteger_var95 = $54
	sta DrawColumn39ToBack_rightvarInteger_var95
	sty DrawColumn39ToBack_rightvarInteger_var95+1
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
	adc DrawColumn39ToBack_rightvarInteger_var95
DrawColumn39ToBack_wordAdd93
	sta DrawColumn39ToBack_rightvarInteger_var95
	; High-bit binop
	tya
	adc DrawColumn39ToBack_rightvarInteger_var95+1
	tay
	lda DrawColumn39ToBack_rightvarInteger_var95
	sta backbuffer_base_ptr
	sty backbuffer_base_ptr+1
	; LineNumber: 202
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 203
	lda #$41
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 204
	lda #$19
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 205
DrawColumn39ToBack_while96
DrawColumn39ToBack_loopstart100
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs DrawColumn39ToBack_elsedoneblock99
DrawColumn39ToBack_ConditionalTrueBlock97: ;Main true block ;keep 
	; LineNumber: 206
	; LineNumber: 208
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (backbuffer_base_ptr),y
	; LineNumber: 209
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39ToBack_WordAdd105
	inc backbuffer_base_ptr+1
DrawColumn39ToBack_WordAdd105
	; LineNumber: 211
	; Test Inc dec D
	inc i
	; LineNumber: 212
	; Test Inc dec D
	inc this_char
	; LineNumber: 214
	jmp DrawColumn39ToBack_while96
DrawColumn39ToBack_elsedoneblock99
DrawColumn39ToBack_loopend101
	; LineNumber: 216
	rts
end_procedure_DrawColumn39ToBack
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 220
swap_screens
	; LineNumber: 222
	jsr DrawColumn39ToBack
	; LineNumber: 224
	
; //addbreakpoint();
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 225
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 227
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne swap_screens_elseblock109
swap_screens_ConditionalTrueBlock108: ;Main true block ;keep 
	; LineNumber: 228
	; LineNumber: 229
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	; LineNumber: 230
	lda #$00
	ldx #$34
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 231
	lda #$1
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 233
	jmp swap_screens_elsedoneblock110
swap_screens_elseblock109
	; LineNumber: 234
	; LineNumber: 235
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 236
	lda #$00
	ldx #$30
	sta current_screen_pointer
	stx current_screen_pointer+1
	; LineNumber: 237
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 238
swap_screens_elsedoneblock110
	; LineNumber: 244
	rts
end_procedure_swap_screens
	
; //SetRegularColorMode();
; //copy_colors(1); 
; // color_shift_lower
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar
	;    Procedure type : User-defined procedure
	; LineNumber: 247
fillwithchar
	; LineNumber: 256
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
fillwithchar_forloop116
	; LineNumber: 250
	; LineNumber: 251
	ldy #$7f ; optimized, look out for bugs
	lda number_1
fillwithchar_fill127
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill127
	; LineNumber: 252
	lda screen_base_ptr
	clc
	adc #$80
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_WordAdd128
	inc screen_base_ptr+1
fillwithchar_WordAdd128
	; LineNumber: 253
	ldy #$7f ; optimized, look out for bugs
	lda number_2
fillwithchar_fill129
	sta (backbuffer_base_ptr),y
	dey
	bpl fillwithchar_fill129
	; LineNumber: 254
	lda backbuffer_base_ptr
	clc
	adc #$80
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_WordAdd130
	inc backbuffer_base_ptr+1
fillwithchar_WordAdd130
	; LineNumber: 255
fillwithchar_forloopcounter118
fillwithchar_loopstart119
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$7
	cmp i ;keep
	bcs fillwithchar_forloop116
fillwithchar_loopdone131: ;keep
fillwithchar_forloopend117
fillwithchar_loopend120
	; LineNumber: 255
	ldy #$68 ; optimized, look out for bugs
	lda number_1
fillwithchar_fill132
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill132
	; LineNumber: 257
	
; // last bit
	ldy #$68 ; optimized, look out for bugs
	lda number_2
fillwithchar_fill133
	sta (backbuffer_base_ptr),y
	dey
	bpl fillwithchar_fill133
	; LineNumber: 259
	rts
end_procedure_fillwithchar
block1
main_block_begin_
	; LineNumber: 262
	sei
	; LineNumber: 263
	
; // System IRQs, not mine.
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop134
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow135
	iny
MainProgram_dtnooverflow135
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop134
	; LineNumber: 265
	
; // $D800 color address, 40 characters per column, 25 rows
screenmemory =  $fe
colormemory =  $fb
	; LineNumber: 266
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 267
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 268
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 269
	lda $d018
	and #%11110001
	ora #4
	sta $d018
	; LineNumber: 270
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 272
	
; //SetRegularColorMode();
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 274
	jsr fillwithchar
	; LineNumber: 277
	
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
	; LineNumber: 278
	jmp * ; loop like (�/%
	; LineNumber: 280
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

