
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
	; LineNumber: 271
	jmp block1
	; LineNumber: 30
screen_base_ptr	= $02
	; LineNumber: 31
backbuffer_base_ptr	= $04
	; LineNumber: 33
row	dc.b	$00
	; LineNumber: 33
col	dc.b	$00
	; LineNumber: 33
current_screen	dc.b	$00
	; LineNumber: 33
i	dc.b	$00
	; LineNumber: 34
number_0	dc.b	$30
	; LineNumber: 35
number_1	dc.b	$31
	; LineNumber: 36
startline	dc.b	$00
	; LineNumber: 37
numlines	dc.b	$00
	; LineNumber: 39
offset	dc.w	0
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
	; LineNumber: 88
irq_line_65
	; LineNumber: 90
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 92
	; Regularcolor mode
	lda $d016
	and #%11101111
	sta $d016
	; LineNumber: 94
	
; //copy_colors(0); 
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	; Signed compare
	bne irq_line_65_elsedoneblock8
irq_line_65_ConditionalTrueBlock6: ;Main true block ;keep 
	; LineNumber: 95
	; LineNumber: 97
irq_line_65_elsedoneblock8
	; LineNumber: 99
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 101
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 103
	rti
end_procedure_irq_line_65
	
; // vblank line starts at 245
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 107
irq_begin_vblank
	; LineNumber: 109
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 109
	; wait for raster
	ldx #$f5 ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 112
	
; // Too quick ATM
	; Test Inc dec D
	dec scroll
	; LineNumber: 114
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	; Signed compare
	bmi irq_begin_vblank_elseblock14
irq_begin_vblank_ConditionalTrueBlock13: ;Main true block ;keep 
	; LineNumber: 114
	; LineNumber: 116
	jsr swap_screens
	; LineNumber: 118
	jmp irq_begin_vblank_elsedoneblock15
irq_begin_vblank_elseblock14
	; LineNumber: 119
	; LineNumber: 121
	
; // 2's complement, >= 127 = negative
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 124
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock35
irq_begin_vblank_ConditionalTrueBlock33: ;Main true block ;keep 
	; LineNumber: 125
	; LineNumber: 126
	
; // Copy top half of char screen to back buffer.
	lda #$0
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 127
	lda #$c
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 128
	jsr copy_and_shift
	; LineNumber: 129
irq_begin_vblank_elsedoneblock35
	; LineNumber: 132
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$2;keep
	; Signed compare
	bne irq_begin_vblank_elsedoneblock41
irq_begin_vblank_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 133
	; LineNumber: 134
	
; // Copy bottom half of char screen to back buffer.
	lda #$c
	; Calling storevariable on generic assign expression
	sta startline
	; LineNumber: 135
	lda #$d
	; Calling storevariable on generic assign expression
	sta numlines
	; LineNumber: 136
	jsr copy_and_shift
	; LineNumber: 138
irq_begin_vblank_elsedoneblock41
	; LineNumber: 139
irq_begin_vblank_elsedoneblock15
	; LineNumber: 142
	
; //hideborderx(1);
; //addbreakpoint();
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<irq_line_65
	sta $fffe
	lda #>irq_line_65
	sta $ffff
	; LineNumber: 145
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 147
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_and_shift
	;    Procedure type : User-defined procedure
	; LineNumber: 150
copy_and_shift
	; LineNumber: 152
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 153
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 155
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
	; LineNumber: 156
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
	; LineNumber: 157
	; LineNumber: 159
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
	; LineNumber: 163
	
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
	; LineNumber: 164
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
	; LineNumber: 165
	lda #$1
	; Calling storevariable on generic assign expression
	sta col
	; LineNumber: 166
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 167
copy_and_shift_while94
copy_and_shift_loopstart98
	; Binary clause Simplified: LESS
	lda col
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs copy_and_shift_elsedoneblock97
copy_and_shift_ConditionalTrueBlock95: ;Main true block ;keep 
	; LineNumber: 168
	; LineNumber: 169
	; 8 bit binop
	; Add/sub where right value is constant number
	lda col
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
	; LineNumber: 170
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne copy_and_shift_elseblock112
copy_and_shift_ConditionalTrueBlock111: ;Main true block ;keep 
	; LineNumber: 171
	; LineNumber: 172
	; Load pointer array
	ldy col
	lda (screen_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (backbuffer_base_ptr),y
	; LineNumber: 174
	jmp copy_and_shift_elsedoneblock113
copy_and_shift_elseblock112
	; LineNumber: 175
	; LineNumber: 176
	; Load pointer array
	ldy col
	lda (backbuffer_base_ptr),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_base_ptr),y
	; LineNumber: 177
copy_and_shift_elsedoneblock113
	; LineNumber: 178
	; Test Inc dec D
	inc col
	; LineNumber: 179
	jmp copy_and_shift_while94
copy_and_shift_elsedoneblock97
copy_and_shift_loopend99
	; LineNumber: 181
	; Test Inc dec D
	inc row
	; LineNumber: 183
	jmp copy_and_shift_while45
copy_and_shift_elsedoneblock48
copy_and_shift_loopend50
	; LineNumber: 185
	rts
end_procedure_copy_and_shift
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 231
swap_screens
	; LineNumber: 235
	
; //addbreakpoint();
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 236
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 238
	; Binary clause Simplified: EQUALS
	clc
	lda current_screen
	; cmp #$00 ignored
	bne swap_screens_elseblock121
swap_screens_ConditionalTrueBlock120: ;Main true block ;keep 
	; LineNumber: 239
	; LineNumber: 240
	lda $d018
	and #%00001111
	ora #208
	sta $d018
	; LineNumber: 241
	lda #$1
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 243
	jmp swap_screens_elsedoneblock122
swap_screens_elseblock121
	; LineNumber: 244
	; LineNumber: 245
	lda $d018
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 246
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 247
swap_screens_elsedoneblock122
	; LineNumber: 253
	rts
end_procedure_swap_screens
	
; //SetRegularColorMode();
; //	DrawColumn39ToBack();
; //copy_colors(1); 
; // color_shift_lower
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fillwithchar
	;    Procedure type : User-defined procedure
	; LineNumber: 256
fillwithchar
	; LineNumber: 265
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
fillwithchar_forloop128
	; LineNumber: 259
	; LineNumber: 260
	ldy #$7f ; optimized, look out for bugs
	lda number_0
fillwithchar_fill139
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill139
	; LineNumber: 261
	lda screen_base_ptr
	clc
	adc #$80
	sta screen_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_WordAdd140
	inc screen_base_ptr+1
fillwithchar_WordAdd140
	; LineNumber: 262
	ldy #$7f ; optimized, look out for bugs
	lda number_1
fillwithchar_fill141
	sta (backbuffer_base_ptr),y
	dey
	bpl fillwithchar_fill141
	; LineNumber: 263
	lda backbuffer_base_ptr
	clc
	adc #$80
	sta backbuffer_base_ptr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc fillwithchar_WordAdd142
	inc backbuffer_base_ptr+1
fillwithchar_WordAdd142
	; LineNumber: 264
fillwithchar_forloopcounter130
fillwithchar_loopstart131
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$7
	cmp i ;keep
	bcs fillwithchar_forloop128
fillwithchar_loopdone143: ;keep
fillwithchar_forloopend129
fillwithchar_loopend132
	; LineNumber: 264
	ldy #$68 ; optimized, look out for bugs
	lda number_0
fillwithchar_fill144
	sta (screen_base_ptr),y
	dey
	bpl fillwithchar_fill144
	; LineNumber: 266
	
; // last bit
	ldy #$68 ; optimized, look out for bugs
	lda number_1
fillwithchar_fill145
	sta (backbuffer_base_ptr),y
	dey
	bpl fillwithchar_fill145
	; LineNumber: 268
	rts
end_procedure_fillwithchar
block1
main_block_begin_
	; LineNumber: 271
	sei
	; LineNumber: 272
	
; // System IRQs, not mine.
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop146
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow147
	iny
MainProgram_dtnooverflow147
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop146
	; LineNumber: 274
	
; // $D800 color address, 40 characters per column, 25 rows
screenmemory =  $fe
colormemory =  $fb
	; LineNumber: 275
	lda #$00
	ldx #$30
	sta screen_base_ptr
	stx screen_base_ptr+1
	; LineNumber: 276
	ldx #$34
	sta backbuffer_base_ptr
	stx backbuffer_base_ptr+1
	; LineNumber: 277
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 278
	lda $d018
	and #%11110001
	ora #4
	sta $d018
	; LineNumber: 279
	and #%00001111
	ora #192
	sta $d018
	; LineNumber: 281
	
; //SetRegularColorMode();
	lda #$0
	; Calling storevariable on generic assign expression
	sta current_screen
	; LineNumber: 283
	jsr fillwithchar
	; LineNumber: 286
	
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
	; LineNumber: 287
	jmp * ; loop like (�/%
	; LineNumber: 289
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

