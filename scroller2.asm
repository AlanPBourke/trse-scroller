
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
MegaScroller
	; LineNumber: 222
	jmp block1
	; LineNumber: 3
Luminosities_fromBlue	dc.b $06, $06, $09, $0b, $02, $04, $08, $0c
	dc.b $0e, $0a, $05, $0f, $03, $07, $0d, $01
	dc.b $0d, $07, $03, $0f, $05, $0a, $0e, $0c
	dc.b $08, $04, $02, $0b, $09, $06, $06, $06
	; LineNumber: 6
dx	dc.b	$00
	; LineNumber: 6
dy	dc.b	$00
	; LineNumber: 6
c	dc.b	$00
	; LineNumber: 6
val	dc.b	$00
	; LineNumber: 6
i	dc.b	$00
	; LineNumber: 6
j	dc.b	$00
	; LineNumber: 6
scroll	dc.b	$00
	; LineNumber: 6
scrollsplit	dc.b	$00
	; LineNumber: 6
val2	dc.b	$00
	; LineNumber: 6
k	dc.b	$00
	; LineNumber: 7
radial	dc.w	0
	; LineNumber: 13
	; LineNumber: 14
	; LineNumber: 16
	; LineNumber: 19
shiftx	dc.b	0
	; LineNumber: 21
text		dc.b	"SCROLL ME TROLL ME    "
	dc.b	0
	; LineNumber: 24
g_currentBank	dc.b	$00
	; LineNumber: 27
textp	= $02
	; LineNumber: 28
zeropage1	=  $04
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4c     ;$59 used for hi-byte
initdiv16x8_dividend = $4e	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4e ;save memory by reusing divident to store the result
divide16x8
	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16:	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16
	dex
	bne divloop16
	rts
end_procedure_init16x8div
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
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4c
div8x8_d = $4e
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1
	rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2
	dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
	rts
end_procedure_init8x8div
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
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
colormemory =  $fb
screen_x = $4c
screen_y = $4e
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #40
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
initmoveto_moveto3
	rts
end_procedure_initmoveto
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initsqrt16
	;    Procedure type : User-defined procedure
sqrt16_init
	ldy #$01 
	sty $50
	dey
	sty $52 ; msby of first odd number (sqrt = 0)
sqrt16_again
	sec
	ldx $4c ; save remainder in X register ; optimized, look out for bugs
	sbc $50
	sta $4c
	lda $4e ; subtract odd hi from integer hi
	sbc $52
	sta $4e ; is subtract result negative?
	bcc sqrt16_nomore ; no. increment square root
	iny
	lda $50 ; calculate next odd number
	adc #$01
	sta $50
	bcc sqrt16_again
	inc $52
	jmp sqrt16_again
sqrt16_nomore
	sty $4c ; all done, store square root
	stx $4e ; and remainder
	rts
end_procedure_initsqrt16
	
; // Place charset on both bank 0 and 1
; // 2x2 font included at some random position
; // Scroll shift
; // Scroller y amplitude
; // ends with a zero
; // Banking variable
; // 
; // use chars 64-72 as fire chars. You could try others here, like 80	g_dataLoc : byte=$1a;
; // Turn ON of off KERNAL(0/1)
; // Define some text pointers
; //	
; //	This tutorial basically takes a 2x2 charset and converts bit information in the character map
; //	to a 16x16 char image.
; //	
; //	In this tutorial, we render off-screen on an alternate bank, and perform half-page
; //	copying on various stages of the scroll. When the scrolling resets, we flip banks and start
; //	over again.
; //	
; //	In addition to this, we cycle the 16 rendering characters using the same method as in previous tutorials
; //	
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShiftCharsetData
	;    Procedure type : User-defined procedure
	; LineNumber: 56
ShiftCharsetData
	; LineNumber: 57
	; memcpy
	ldx #0
ShiftCharsetData_memcpy5
	lda $2b40+ $00,x
	sta $2bc0,x
	inx
	cpx #$8
	bne ShiftCharsetData_memcpy5
	; LineNumber: 60
	; memcpy
	ldx #0
ShiftCharsetData_memcpy6
	lda $2b48+ $00,x
	sta $2b40,x
	inx
	cpx #$80
	bne ShiftCharsetData_memcpy6
	; LineNumber: 61
	; memcpy
	ldx #0
ShiftCharsetData_memcpy7
	lda $6b40+ $00,x
	sta $6bc0,x
	inx
	cpx #$8
	bne ShiftCharsetData_memcpy7
	; LineNumber: 62
	; memcpy
	ldx #0
ShiftCharsetData_memcpy8
	lda $6b48+ $00,x
	sta $6b40,x
	inx
	cpx #$80
	bne ShiftCharsetData_memcpy8
	; LineNumber: 63
	rts
end_procedure_ShiftCharsetData
	
; //	The following method copies half upper/lower screen to 1 char left
; //	
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CopyHScreen
	;    Procedure type : User-defined procedure
	; LineNumber: 70
	; LineNumber: 69
ul_	dc.b	0
CopyHScreen_block9
CopyHScreen
	; LineNumber: 71
	; Binary clause Simplified: EQUALS
	clc
	lda g_currentBank
	; cmp #$00 ignored
	bne CopyHScreen_localfailed32
	jmp CopyHScreen_ConditionalTrueBlock11
CopyHScreen_localfailed32
	jmp CopyHScreen_elsedoneblock13
CopyHScreen_ConditionalTrueBlock11: ;Main true block ;keep 
	; LineNumber: 71
	; LineNumber: 72
	; Binary clause Simplified: EQUALS
	clc
	lda ul_
	; cmp #$00 ignored
	bne CopyHScreen_localfailed44
	jmp CopyHScreen_ConditionalTrueBlock35
CopyHScreen_localfailed44
	jmp CopyHScreen_elseblock36
CopyHScreen_ConditionalTrueBlock35: ;Main true block ;keep 
	; LineNumber: 72
	; Copy half screen unrolled 500 bytes = 12.5*40
	ldx #00
CopyHScreen_halfcopyloop46
	lda $608 + 320 -1 ,x
	sta $4607 + 320 -1 ,x
	lda $608 + 280 -1 ,x
	sta $4607 + 280 -1 ,x
	lda $608 + 240 -1 ,x
	sta $4607 + 240 -1 ,x
	lda $608 + 200 -1 ,x
	sta $4607 + 200 -1 ,x
	lda $608 + 160 -1 ,x
	sta $4607 + 160 -1 ,x
	lda $608 + 120 -1 ,x
	sta $4607 + 120 -1 ,x
	lda $608 + 80 -1 ,x
	sta $4607 + 80 -1 ,x
	lda $608 + 40 -1 ,x
	sta $4607 + 40 -1 ,x
	lda $608 + 0 -1 ,x
	sta $4607 + 0 -1 ,x
	inx
	cpx #40
	bne CopyHScreen_halfcopyloop46
	jmp CopyHScreen_elsedoneblock37
CopyHScreen_elseblock36
	; LineNumber: 74
	; Copy half screen unrolled 500 bytes = 12.5*40
	ldx #00
CopyHScreen_halfcopyloop49
	lda $400 + 480 -1 ,x
	sta $43ff + 480 -1 ,x
	lda $400 + 440 -1 ,x
	sta $43ff + 440 -1 ,x
	lda $400 + 400 -1 ,x
	sta $43ff + 400 -1 ,x
	lda $400 + 360 -1 ,x
	sta $43ff + 360 -1 ,x
	lda $400 + 320 -1 ,x
	sta $43ff + 320 -1 ,x
	lda $400 + 280 -1 ,x
	sta $43ff + 280 -1 ,x
	lda $400 + 240 -1 ,x
	sta $43ff + 240 -1 ,x
	lda $400 + 200 -1 ,x
	sta $43ff + 200 -1 ,x
	lda $400 + 160 -1 ,x
	sta $43ff + 160 -1 ,x
	lda $400 + 120 -1 ,x
	sta $43ff + 120 -1 ,x
	lda $400 + 80 -1 ,x
	sta $43ff + 80 -1 ,x
	lda $400 + 40 -1 ,x
	sta $43ff + 40 -1 ,x
	lda $400 + 0 -1 ,x
	sta $43ff + 0 -1 ,x
	inx
	cpx #40
	bne CopyHScreen_halfcopyloop49
CopyHScreen_elsedoneblock37
	; LineNumber: 77
CopyHScreen_elsedoneblock13
	; LineNumber: 78
	; Binary clause Simplified: EQUALS
	lda g_currentBank
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne CopyHScreen_localfailed73
	jmp CopyHScreen_ConditionalTrueBlock52
CopyHScreen_localfailed73
	jmp CopyHScreen_elsedoneblock54
CopyHScreen_ConditionalTrueBlock52: ;Main true block ;keep 
	; LineNumber: 78
	; LineNumber: 79
	; Binary clause Simplified: EQUALS
	clc
	lda ul_
	; cmp #$00 ignored
	bne CopyHScreen_localfailed85
	jmp CopyHScreen_ConditionalTrueBlock76
CopyHScreen_localfailed85
	jmp CopyHScreen_elseblock77
CopyHScreen_ConditionalTrueBlock76: ;Main true block ;keep 
	; LineNumber: 79
	; Copy half screen unrolled 500 bytes = 12.5*40
	ldx #00
CopyHScreen_halfcopyloop87
	lda $4608 + 320 -1 ,x
	sta $607 + 320 -1 ,x
	lda $4608 + 280 -1 ,x
	sta $607 + 280 -1 ,x
	lda $4608 + 240 -1 ,x
	sta $607 + 240 -1 ,x
	lda $4608 + 200 -1 ,x
	sta $607 + 200 -1 ,x
	lda $4608 + 160 -1 ,x
	sta $607 + 160 -1 ,x
	lda $4608 + 120 -1 ,x
	sta $607 + 120 -1 ,x
	lda $4608 + 80 -1 ,x
	sta $607 + 80 -1 ,x
	lda $4608 + 40 -1 ,x
	sta $607 + 40 -1 ,x
	lda $4608 + 0 -1 ,x
	sta $607 + 0 -1 ,x
	inx
	cpx #40
	bne CopyHScreen_halfcopyloop87
	jmp CopyHScreen_elsedoneblock78
CopyHScreen_elseblock77
	; LineNumber: 81
	; Copy half screen unrolled 500 bytes = 12.5*40
	ldx #00
CopyHScreen_halfcopyloop90
	lda $4400 + 480 -1 ,x
	sta $3ff + 480 -1 ,x
	lda $4400 + 440 -1 ,x
	sta $3ff + 440 -1 ,x
	lda $4400 + 400 -1 ,x
	sta $3ff + 400 -1 ,x
	lda $4400 + 360 -1 ,x
	sta $3ff + 360 -1 ,x
	lda $4400 + 320 -1 ,x
	sta $3ff + 320 -1 ,x
	lda $4400 + 280 -1 ,x
	sta $3ff + 280 -1 ,x
	lda $4400 + 240 -1 ,x
	sta $3ff + 240 -1 ,x
	lda $4400 + 200 -1 ,x
	sta $3ff + 200 -1 ,x
	lda $4400 + 160 -1 ,x
	sta $3ff + 160 -1 ,x
	lda $4400 + 120 -1 ,x
	sta $3ff + 120 -1 ,x
	lda $4400 + 80 -1 ,x
	sta $3ff + 80 -1 ,x
	lda $4400 + 40 -1 ,x
	sta $3ff + 40 -1 ,x
	lda $4400 + 0 -1 ,x
	sta $3ff + 0 -1 ,x
	inx
	cpx #40
	bne CopyHScreen_halfcopyloop90
CopyHScreen_elsedoneblock78
	; LineNumber: 83
CopyHScreen_elsedoneblock54
	; LineNumber: 85
	rts
end_procedure_CopyHScreen
	
; //Switches between bank 0 and 1
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : SwitchBank
	;    Procedure type : User-defined procedure
	; LineNumber: 91
SwitchBank
	; LineNumber: 92
	; Binary clause Simplified: EQUALS
	clc
	lda g_currentBank
	; cmp #$00 ignored
	bne SwitchBank_elseblock95
SwitchBank_ConditionalTrueBlock94: ;Main true block ;keep 
	; LineNumber: 92
	; Set bank
	lda #$2
	sta $dd00
	jmp SwitchBank_elsedoneblock96
SwitchBank_elseblock95
	; LineNumber: 94
	; Set bank
	lda #$3
	sta $dd00
SwitchBank_elsedoneblock96
	; LineNumber: 96
	lda $d018
	and #%11110001
	ora #10
	sta $d018
	; LineNumber: 99
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda g_currentBank
	clc
	adc #$1
	 ; end add / sub var with constant
	and #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta g_currentBank
	; LineNumber: 100
	rts
end_procedure_SwitchBank
	
; //Performs the rendering of the 16x16 text, split into 1x8 + 1x8 vertical lines for each stage(shiftx)
; //	
	; NodeProcedureDecl -1
	; ***********  Defining procedure : WriteText
	;    Procedure type : User-defined procedure
	; LineNumber: 108
WriteText
	; LineNumber: 110
	lda #>$400
	; Calling storevariable on generic assign expression
	sta val
	; LineNumber: 111
	; Binary clause Simplified: EQUALS
	clc
	lda g_currentBank
	; cmp #$00 ignored
	bne WriteText_elsedoneblock105
WriteText_ConditionalTrueBlock103: ;Main true block ;keep 
	; LineNumber: 110
	; Optimizer: a = a +/- b
	lda val
	clc
	adc #$40
	sta val
WriteText_elsedoneblock105
	; LineNumber: 114
	; MoveTo optimization
	lda #$a0
	sta screenmemory
	lda val
	clc
	adc #$00
	sta screenmemory+1
	; LineNumber: 116
	
; // Clear the single byte on the rightmost column
	lda #$20
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (screenmemory),y
	; LineNumber: 117
	; MoveTo optimization
	lda #$c8
	sta screenmemory
	lda val
	clc
	adc #$00
	sta screenmemory+1
	; LineNumber: 119
	; Binary clause Simplified: GREATEREQUAL
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	bcc WriteText_localfailed259
	jmp WriteText_ConditionalTrueBlock109
WriteText_localfailed259
	jmp WriteText_elsedoneblock111
WriteText_ConditionalTrueBlock109: ;Main true block ;keep 
	; LineNumber: 118
	; LineNumber: 120
	
; // if scroll<0, trigger a new line to draw
	; Load pointer array
	ldy #$0
	lda (textp),y
	; Calling storevariable on generic assign expression
	sta val
	; LineNumber: 121
	; Binary clause Simplified: GREATEREQUAL
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcc WriteText_elsedoneblock264
WriteText_ConditionalTrueBlock262: ;Main true block ;keep 
	; LineNumber: 120
	
; // Look up current char
	; Optimizer: a = a +/- b
	lda val
	sec
	sbc #$41
	sta val
WriteText_elsedoneblock264
	; LineNumber: 123
	
; // Look up in charmap
	lda #<font
	ldx #>font
	sta zeropage1
	stx zeropage1+1
	; LineNumber: 124
	
; // Point to an empty location in font space
	; Binary clause Simplified: EQUALS
	lda val
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne WriteText_elsedoneblock270
WriteText_ConditionalTrueBlock268: ;Main true block ;keep 
	; LineNumber: 124
	; LineNumber: 125
	lda #$40
	; Calling storevariable on generic assign expression
	sta val
	; LineNumber: 126
WriteText_elsedoneblock270
	; LineNumber: 127
	; Binary clause Simplified: GREATEREQUAL
	lda val
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcc WriteText_elsedoneblock276
WriteText_ConditionalTrueBlock274: ;Main true block ;keep 
	; LineNumber: 126
	; Optimizer: a = a +/- b
	lda val
	clc
	adc #$18
	sta val
WriteText_elsedoneblock276
	; LineNumber: 130
	
; // Align chars(for this particular font)
; // point to the correct char in memory, each char is 2x2 x 8 bytes, so 16 bytes in total
	; Generic 16 bit op
	; integer assignment NodeVar
	ldy zeropage1+1 ; keep
	lda zeropage1
WriteText_rightvarInteger_var281 = $54
	sta WriteText_rightvarInteger_var281
	sty WriteText_rightvarInteger_var281+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda val
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$10
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc WriteText_rightvarInteger_var281
WriteText_wordAdd279
	sta WriteText_rightvarInteger_var281
	; High-bit binop
	tya
	adc WriteText_rightvarInteger_var281+1
	tay
	lda WriteText_rightvarInteger_var281
	sta zeropage1
	sty zeropage1+1
	; LineNumber: 134
	
; // val2 here is the actual character to draw, 74 is semi-filled char
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #113
	sec
	sbc shiftx
	 ; end add / sub var with constant
	clc
	adc scrollsplit
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta val2
	; LineNumber: 136
	
; // Scrollsplit decides whether we use the first(0) char in a letter or the second(8)
	lda zeropage1
	clc
	adc scrollsplit
	sta zeropage1+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc WriteText_WordAdd282
	inc zeropage1+1
WriteText_WordAdd282
	; LineNumber: 151
	lda #$0
	; Calling storevariable on generic assign expression
	sta j
WriteText_forloop283
	; LineNumber: 139
	; LineNumber: 148
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
WriteText_forloop323
	; LineNumber: 140
	; LineNumber: 142
	
; // Draw upper and lower 8 chars
; // Default is blank
	lda #$20
	; Calling storevariable on generic assign expression
	sta c
	; LineNumber: 143
	; Binary clause Simplified: EQUALS
	ldx shiftx ; optimized, look out for bugs
	lda #1
WriteText_shiftbit349
	cpx #0
	beq WriteText_shiftbitdone350
	asl
	dex
	jmp WriteText_shiftbit349
WriteText_shiftbitdone350
WriteText_bitmask_var351 = $54
	sta WriteText_bitmask_var351
	; Load pointer array
	ldy i
	lda (zeropage1),y
	and WriteText_bitmask_var351
	cmp WriteText_bitmask_var351
	bne WriteText_getbit_false347
	lda #1
	jmp WriteText_getbit_done348
WriteText_getbit_false347
	lda #0
WriteText_getbit_done348
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne WriteText_elsedoneblock345
WriteText_ConditionalTrueBlock343: ;Main true block ;keep 
	; LineNumber: 142
	lda val2
	; Calling storevariable on generic assign expression
	sta c
WriteText_elsedoneblock345
	; LineNumber: 144
	lda c
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (screenmemory),y
	; LineNumber: 145
	lda screenmemory
	clc
	adc #$28
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc WriteText_WordAdd353
	inc screenmemory+1
WriteText_WordAdd353
	; LineNumber: 147
WriteText_forloopcounter325
WriteText_loopstart326
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$8
	cmp i ;keep
	bne WriteText_forloop323
WriteText_loopdone354: ;keep
WriteText_forloopend324
WriteText_loopend327
	; LineNumber: 149
	
; // Increase 8 rows(8*40=320)
	lda zeropage1
	clc
	adc #$40
	sta zeropage1+0
	lda zeropage1+1
	adc #$01
	sta zeropage1+1
	; LineNumber: 150
WriteText_forloopcounter285
WriteText_loopstart286
	; Compare is onpage
	; Test Inc dec D
	inc j
	lda #$2
	cmp j ;keep
	bne WriteText_forloop283
WriteText_loopdone356: ;keep
WriteText_forloopend284
WriteText_loopend287
	; LineNumber: 151
	; Test Inc dec D
	dec shiftx
	; LineNumber: 152
	; Binary clause Simplified: GREATEREQUAL
	lda shiftx
	; Compare with pure num / var optimization
	cmp #$7f;keep
	bcc WriteText_elsedoneblock360
WriteText_ConditionalTrueBlock358: ;Main true block ;keep 
	; LineNumber: 152
	; LineNumber: 154
	
; // if shift is less than zero
	lda #$7
	; Calling storevariable on generic assign expression
	sta shiftx
	; LineNumber: 156
	; Binary clause Simplified: EQUALS
	clc
	lda scrollsplit
	; cmp #$00 ignored
	bne WriteText_elseblock387
WriteText_ConditionalTrueBlock386: ;Main true block ;keep 
	; LineNumber: 155
	
; // Reset value
; // Do we increase text pointer or just shift +8 bytes to draw next part of 2x2 char?
	lda #$8
	; Calling storevariable on generic assign expression
	sta scrollsplit
	jmp WriteText_elsedoneblock388
WriteText_elseblock387
	; LineNumber: 157
	; LineNumber: 159
	
; // Reset and increase text pointer!
	lda #$0
	; Calling storevariable on generic assign expression
	sta scrollsplit
	; LineNumber: 160
	lda textp
	clc
	adc #$01
	sta textp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc WriteText_WordAdd400
	inc textp+1
WriteText_WordAdd400
	; LineNumber: 161
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	ldy #$0
	lda (textp),y
	; cmp #$00 ignored
	bne WriteText_elsedoneblock404
WriteText_ConditionalTrueBlock402: ;Main true block ;keep 
	; LineNumber: 160
	lda #<text
	ldx #>text
	sta textp
	stx textp+1
WriteText_elsedoneblock404
	; LineNumber: 162
WriteText_elsedoneblock388
	; LineNumber: 163
WriteText_elsedoneblock360
	; LineNumber: 165
	
; // Reset scroll
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 166
WriteText_elsedoneblock111
	; LineNumber: 169
	rts
end_procedure_WriteText
	
; //	Main raster interrupt.
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : RasterMain
	;    Procedure type : User-defined procedure
	; LineNumber: 175
RasterMain
	; LineNumber: 176
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 178
	; Optimizer: a = a +/- b
	lda scroll
	sec
	sbc #$2
	sta scroll
	; LineNumber: 179
	
; // Scroll speed
	; ScrollX method
	sta  $58
	lda $d016  
	and #$F8
	ora  $58
	sta $d016
	; LineNumber: 180
	; Regularcolor mode
	and #%11101111
	sta $d016
	; LineNumber: 182
	
; // Prints out the current text on columm 39	
	jsr WriteText
	; LineNumber: 184
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne RasterMain_elsedoneblock411
RasterMain_ConditionalTrueBlock409: ;Main true block ;keep 
	; LineNumber: 183
	
; // If scroll=1 or 3, copy upper or lower screen to the other bank
	lda #$0
	; Calling storevariable on generic assign expression
	sta ul_
	jsr CopyHScreen
RasterMain_elsedoneblock411
	; LineNumber: 185
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne RasterMain_elsedoneblock417
RasterMain_ConditionalTrueBlock415: ;Main true block ;keep 
	; LineNumber: 184
	lda #$1
	; Calling storevariable on generic assign expression
	sta ul_
	jsr CopyHScreen
RasterMain_elsedoneblock417
	; LineNumber: 187
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7;keep
	bne RasterMain_elsedoneblock423
RasterMain_ConditionalTrueBlock421: ;Main true block ;keep 
	; LineNumber: 186
	
; // End of scroll: switch banks, reset scroll
	jsr SwitchBank
RasterMain_elsedoneblock423
	; LineNumber: 189
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$7;keep
	bne RasterMain_localfailed431
	jmp RasterMain_ConditionalTrueBlock427
RasterMain_localfailed431: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne RasterMain_elsedoneblock429
RasterMain_ConditionalTrueBlock427: ;Main true block ;keep 
	; LineNumber: 189
	
; // Shift us some charset data 50% of the frames. For good looks. 	
	jsr ShiftCharsetData
RasterMain_elsedoneblock429
	; LineNumber: 192
	lda $D016
	and #%11110111
	sta $D016
	; LineNumber: 194
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 195
	rti
end_procedure_RasterMain
	; Empty NMI 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : NMI
	;    Procedure type : User-defined procedure
	; LineNumber: 199
NMI
	; LineNumber: 200
	rti
end_procedure_NMI
	
; // Fill the screen with purdy colors
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitColors
	;    Procedure type : User-defined procedure
	; LineNumber: 204
InitColors
	; LineNumber: 205
	lda #$00
	ldx #$d8
	sta zeropage1
	stx zeropage1+1
	; LineNumber: 220
	lda #$0
	; Calling storevariable on generic assign expression
	sta k
InitColors_forloop435
	; LineNumber: 207
	; LineNumber: 218
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitColors_forloop462
	; LineNumber: 209
	; LineNumber: 210
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$14
	sec
	sbc i
	 ; end add / sub var with constant
	cmp #127
	bcc InitColors_abslabel474
	eor #$ff
	adc #$00
InitColors_abslabel474
	; Calling storevariable on generic assign expression
	sta dx
	; LineNumber: 211
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$d
	sec
	sbc k
	 ; end add / sub var with constant
	cmp #127
	bcc InitColors_abslabel475
	eor #$ff
	adc #$00
InitColors_abslabel475
	; Calling storevariable on generic assign expression
	sta dy
	; LineNumber: 213
	
; // Calculate the "tangential" value of dx,dy. Try to plot this value indepenedntly!
	; Generic 16 bit op
	ldy #0
	; Mul 16x8 setup
	sta mul16x8_num1
	sty mul16x8_num1Hi
	sta mul16x8_num2
	jsr mul16x8_procedure
InitColors_rightvarInteger_var478 = $54
	sta InitColors_rightvarInteger_var478
	sty InitColors_rightvarInteger_var478+1
	; Mul 16x8 setup
	ldy #0
	lda dx
	sta mul16x8_num1
	sty mul16x8_num1Hi
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc InitColors_rightvarInteger_var478
InitColors_wordAdd476
	sta InitColors_rightvarInteger_var478
	; High-bit binop
	tya
	adc InitColors_rightvarInteger_var478+1
	tay
	lda InitColors_rightvarInteger_var478
	; Calling storevariable on generic assign expression
	sta radial
	sty radial+1
	; LineNumber: 214
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	; integer assignment NodeVar
	ldy radial+1 ; keep
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	lda #$3
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	ldy initdiv16x8_dividend+1
	; Calling storevariable on generic assign expression
	sta radial
	sty radial+1
	; LineNumber: 215
	; Setup sqrt
	; integer assignment NodeVar
	ldy radial+1 ; keep
	sta $4c
	sty $4e
	jsr sqrt16_init
	lda radial
	; Calling storevariable on generic assign expression
	sta val
	; LineNumber: 216
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	and #$1f
	 ; end add / sub var with constant
	tax
	lda Luminosities_fromBlue,x
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (zeropage1),y
	; LineNumber: 217
InitColors_forloopcounter464
InitColors_loopstart465
	; Test Inc dec D
	inc i
	lda #$28
	cmp i ;keep
	beq InitColors_loopdone479
InitColors_loopnotdone480
	jmp InitColors_forloop462
InitColors_loopdone479
InitColors_forloopend463
InitColors_loopend466
	; LineNumber: 218
	lda zeropage1
	clc
	adc #$28
	sta zeropage1+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc InitColors_WordAdd481
	inc zeropage1+1
InitColors_WordAdd481
	; LineNumber: 219
InitColors_forloopcounter437
InitColors_loopstart438
	; Test Inc dec D
	inc k
	lda #$19
	cmp k ;keep
	beq InitColors_loopdone482
InitColors_loopnotdone483
	jmp InitColors_forloop435
InitColors_loopdone482
InitColors_forloopend436
InitColors_loopend439
	; LineNumber: 220
	rts
end_procedure_InitColors
block1
main_block_begin_
	; LineNumber: 224
	
; // Set color background
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d020
	; LineNumber: 225
	; Assigning memory location
	; Calling storevariable on generic assign expression
	sta $d021
	; LineNumber: 227
	
; // Clear screen and color memory
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop484
	dex
	sta $0000+$4400,x
	sta $00fa+$4400,x
	sta $01f4+$4400,x
	sta $02ee+$4400,x
	bne MainProgram_clearloop484
	; LineNumber: 228
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop485
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop485
	; LineNumber: 230
	
; // Fill colors
	jsr InitColors
	; LineNumber: 232
	
; // Initialize text pointer	
	lda #<text
	ldx #>text
	sta textp
	stx textp+1
	; LineNumber: 234
	lda #$7
	; Calling storevariable on generic assign expression
	sta shiftx
	; LineNumber: 235
	lda #$0
	; Calling storevariable on generic assign expression
	sta scrollsplit
	; LineNumber: 237
	lda $D016
	ora #%1000
	sta $D016
	; LineNumber: 239
	; Hook NMI
	sei
	lda     #<NMI
	sta     $0318
	lda     #>NMI
	sta     $0319
	lda     #$00            ; Stop time A CIA2
	sta     $dd0e
	sta     $dd04           ; Set timer value #1 (Timer A CIA 2)
	sta     $dd05           ; Set timer value #2 (Timer A CIA 2)
	lda     #%10000001      ; Fill bit to 1 and enable NMI to occur from Timer A
	sta     $dd0d
	lda     #$01
	sta     $dd0e           ; Start timer A CIA (NMI will occur immediately)(*)
	cli
	; LineNumber: 240
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 241
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; RasterIRQ : Hook a procedure
	lda #$1
	sta $d012
	lda #<RasterMain
	sta $fffe
	lda #>RasterMain
	sta $ffff
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	; LineNumber: 242
	jmp * ; loop like (�/%
	; LineNumber: 243
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:
	org $2800
StartBlock2800:
	org $2800
charset:
	incbin	 "C:/Users/alan/dev/trse-scroller///resources/charsets/charset.bin"
end_incbin_charset:
EndBlock2800:
	org $6800
StartBlock6800:
	org $6800
charset1_2:
	incbin	 "C:/Users/alan/dev/trse-scroller///resources/charsets/charset.bin"
end_incbin_charset1_2:
EndBlock6800:
	org $7000
StartBlock7000:
	org $7000
font:
	incbin	 "C:/Users/alan/dev/trse-scroller///resources/charsets/charset_16x16.bin"
end_incbin_font:
EndBlock7000:

