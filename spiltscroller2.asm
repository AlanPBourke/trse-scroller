
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
	; LineNumber: 259
	jmp block1
	; LineNumber: 30
screen_base_ptr	= $02
	; LineNumber: 31
backbuffer_base_ptr	= $04
	; LineNumber: 33
current_screen	dc.b	$00
	; LineNumber: 33
i	dc.b	$00
	; LineNumber: 34
number_1	dc.b	$31
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
	; LineNumber: 108
irq_line_65
	; LineNumber: 110
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 112
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 114
	
; //copy_colors(0); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock7
irq_line_65_ConditionalTrueBlock5: ;Main true block ;keep 
	; LineNumber: 115
	; LineNumber: 117
irq_line_65_elsedoneblock7
	; LineNumber: 119
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 121
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 123
	rti
end_procedure_irq_line_65
	
; // vblank line starts at 245
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 127
irq_begin_vblank
	; LineNumber: 129
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 129
	; wait for raster
	ldx #$f5 ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 133
	
; // Too quick ATM
; //addbreakpoint();		
	; Test Inc dec D
	dec scroll
	; LineNumber: 135
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock13
irq_begin_vblank_ConditionalTrueBlock12: ;Main true block ;keep 
	; LineNumber: 135
	; LineNumber: 137
	jsr swap_screens
	; LineNumber: 139
	jmp irq_begin_vblank_elsedoneblock14
irq_begin_vblank_elseblock13
	; LineNumber: 140
	; LineNumber: 142
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 145
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock34
irq_begin_vblank_ConditionalTrueBlock32: ;Main true block ;keep 
	; LineNumber: 146
	; LineNumber: 147
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 148
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 149
	jsr copy_and_shift
	; LineNumber: 150
irq_begin_vblank_elsedoneblock34
	; LineNumber: 153
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock40
irq_begin_vblank_ConditionalTrueBlock38: ;Main true block ;keep 
	; LineNumber: 154
	; LineNumber: 155
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 156
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 157
	jsr copy_and_shift
	; LineNumber: 159
irq_begin_vblank_elsedoneblock40
	; LineNumber: 160
irq_begin_vblank_elsedoneblock14
	; LineNumber: 162
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 163
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 166
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 168
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 171
copy_and_shift
	; LineNumber: 173
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$30
	lda #$00
copy_and_shift_rightvarInteger_var46 = $54
	sta copy_and_shift_rightvarInteger_var46
	sty copy_and_shift_rightvarInteger_var46+1
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
	; LineNumber: 174
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$34
	lda #$00
copy_and_shift_rightvarInteger_var49 = $54
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
	sta backbuffer_base_ptr
	sty backbuffer_base_ptr+1
	; LineNumber: 176
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 177
copy_and_shift_while50
copy_and_shift_loopstart54
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs copy_and_shift_elsedoneblock53
copy_and_shift_ConditionalTrueBlock51: ;Main true block ;keep 
	; LineNumber: 178
	; LineNumber: 180
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock74
copy_and_shift_ConditionalTrueBlock73: ;Main true block ;keep 
	; LineNumber: 181
	; LineNumber: 182
	; memcpy
	ldy #0
copy_and_shift_memcpy81
	lda (screen_base_ptr),y
	sta (backbuffer_base_ptr),y
	iny
	cpy #$27
	bne copy_and_shift_memcpy81
	; LineNumber: 184
	jmp copy_and_shift_elsedoneblock75
copy_and_shift_elseblock74
	; LineNumber: 185
	; LineNumber: 186
	; memcpy
	ldy #0
copy_and_shift_memcpy83
	lda (backbuffer_base_ptr),y
	sta (screen_base_ptr),y
	iny
	cpy #$27
	bne copy_and_shift_memcpy83
	; LineNumber: 187
copy_and_shift_elsedoneblock75
	; LineNumber: 189
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd84
	inc screen_base_ptr+1
copy_and_shift_WordAdd84
	; LineNumber: 190
	lda backbuffer_base_ptr
	clc
	adc #$28
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc copy_and_shift_WordAdd85
	inc backbuffer_base_ptr+1
copy_and_shift_WordAdd85
	; LineNumber: 192
	; Test Inc dec D
	inc i
	; LineNumber: 194
	jmp copy_and_shift_while50
copy_and_shift_elsedoneblock53
copy_and_shift_loopend55
	; LineNumber: 195
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39ToFront
	;    Procedure type : User-defined procedure
	; LineNumber: 198
DrawColumn39ToFront
	; LineNumber: 201
	
; // on the back buffer.
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 203
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 204
	lda #$41
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 205
DrawColumn39ToFront_while87
DrawColumn39ToFront_loopstart91
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp numlines;keep
	bcs DrawColumn39ToFront_elsedoneblock90
DrawColumn39ToFront_ConditionalTrueBlock88: ;Main true block ;keep 
	; LineNumber: 206
	; LineNumber: 208
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (screen_base_ptr),y
	; LineNumber: 209
	lda screen_base_ptr
	clc
	adc #$28
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39ToFront_WordAdd96
	inc screen_base_ptr+1
DrawColumn39ToFront_WordAdd96
	; LineNumber: 211
	; Test Inc dec D
	inc i
	; LineNumber: 212
	; Test Inc dec D
	inc this_char
	; LineNumber: 214
	jmp DrawColumn39ToFront_while87
DrawColumn39ToFront_elsedoneblock90
DrawColumn39ToFront_loopend92
	; LineNumber: 216
	rts
end_procedure_DrawColumn39ToFront
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 219
swap_screens
	; LineNumber: 221
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 222
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 224
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne swap_screens_elseblock100
swap_screens_ConditionalTrueBlock99: ;Main true block ;keep 
	; LineNumber: 225
	; LineNumber: 226
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	; LineNumber: 227
	lda #$1
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 229
	jmp swap_screens_elsedoneblock101
swap_screens_elseblock100
	; LineNumber: 230
	; LineNumber: 231
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 232
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 233
swap_screens_elsedoneblock101
	; LineNumber: 238
	
; //SetRegularColorMode();
; //copy_colors(1); 
; // color_shift_lower
	jsr DrawColumn39ToFront
	; LineNumber: 241
	rts
end_procedure_swap_screens
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar
	;    Procedure type : User-defined procedure
	; LineNumber: 244
fillwithchar
	; LineNumber: 253
	
; //FillFast(backbuffer_base_ptr, number_2, 127);
; //backbuffer_base_ptr := backbuffer_base_ptr + 128;			
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
fillwithchar_forloop107
	; LineNumber: 247
	; LineNumber: 248
	ldy #$7f ; optimized, look out for bugs
	lda number_1
fillwithchar_fill116
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill116
	; LineNumber: 249
	lda screen_base_ptr
	clc
	adc #$80
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_WordAdd117
	inc screen_base_ptr+1
fillwithchar_WordAdd117
	; LineNumber: 252
fillwithchar_forloopcounter109
fillwithchar_loopstart110
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$7
	cmp i ;keep
	bcs fillwithchar_forloop107
fillwithchar_loopdone118: ;keep
fillwithchar_forloopend108
fillwithchar_loopend111
	; LineNumber: 252
	ldy #$68 ; optimized, look out for bugs
	lda number_1
fillwithchar_fill119
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill119
	; LineNumber: 256
	rts
end_procedure_fillwithchar
block1
main_block_begin_
	; LineNumber: 259
	
; // last bit
; //FillFast(backbuffer_base_ptr, number_2, 104);
	sei
	; LineNumber: 260
	
; // System IRQs, not mine.
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop120
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow121
	iny
MainProgram_dtnooverflow121
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop120
	; LineNumber: 262
	
; // $D800 color address, 40 characters per column, 25 rows
screenmemory =  $fe
colormemory =  $fb
	; LineNumber: 263
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 264
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 265
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 266
	lda $d018
	and #%11110001
	ora #4
	sta $d018
	; LineNumber: 267
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 269
	
; //SetRegularColorMode();
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 271
	jsr fillwithchar
	; LineNumber: 274
	
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
	; LineNumber: 275
	jmp * ; loop like (�/%
	; LineNumber: 277
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

