
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
SplitScroller
	; LineNumber: 172
	jmp block1
	; LineNumber: 20
Key_KeyRow = $dc00
	; LineNumber: 21
Key_KeyRead = $dc01
	; LineNumber: 23
Key_DataDirA = $dc02
	; LineNumber: 24
Key_DataDirB = $dc03
	; LineNumber: 24
val	dc.b	$00
	; LineNumber: 24
row	dc.b	$00
	; LineNumber: 24
col	dc.b	$00
	; LineNumber: 24
startrow	dc.b	$00
	; LineNumber: 24
endrow	dc.b	$00
	; LineNumber: 24
screen_buffer_num	dc.b	$00
	; LineNumber: 25
this_color	dc.b	$01
	; LineNumber: 25
this_char	dc.b	$01
	; LineNumber: 26
scroll	dc.b	$07
	; LineNumber: 28
screen_base	dc.w	$400
	; LineNumber: 29
screen_backbuffer_base	dc.w	$800
	; LineNumber: 31
caddr	dc.w	 
	org caddr+50
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
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
initmoveto_moveto2
	rts
end_procedure_initmoveto
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_line_65
	;    Procedure type : User-defined procedure
	; LineNumber: 41
irq_line_65
	; LineNumber: 43
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 45
	
; // color_shift_upper
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	bne irq_line_65_elsedoneblock7
irq_line_65_ConditionalTrueBlock5: ;Main true block ;keep 
	; LineNumber: 46
	; LineNumber: 47
	lda #$0
	; Calling storevariable on generic assign expression
	sta ul
	jsr copy_colors
	; LineNumber: 48
irq_line_65_elsedoneblock7
	; LineNumber: 50
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<irq_begin_vblank
	sta $fffe
	lda #>irq_begin_vblank
	sta $ffff
	; LineNumber: 52
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 54
	rti
end_procedure_irq_line_65
	
; // vblank line starts at 245
	; NodeProcedureDecl -1
	; ***********  Defining procedure : irq_begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 58
irq_begin_vblank
	; LineNumber: 60
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 62
	; Test Inc dec D
	dec scroll
	; LineNumber: 64
	; Optimization: replacing a <= N with a <= N-1
	; Binary clause Simplified: LESS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcs irq_begin_vblank_elsedoneblock14
irq_begin_vblank_ConditionalTrueBlock12: ;Main true block ;keep 
	; LineNumber: 65
	; LineNumber: 66
	jsr swap_screens
	; LineNumber: 67
irq_begin_vblank_elsedoneblock14
	; LineNumber: 69
	lda scroll
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 71
	; Binary clause Simplified: EQUALS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne irq_begin_vblank_elsedoneblock20
irq_begin_vblank_ConditionalTrueBlock18: ;Main true block ;keep 
	; LineNumber: 72
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
irq_begin_vblank_elsedoneblock20
	; LineNumber: 75
	rti
end_procedure_irq_begin_vblank
	; NodeProcedureDecl -1
	; ***********  Defining procedure : swap_screens
	;    Procedure type : User-defined procedure
	; LineNumber: 78
swap_screens
	; LineNumber: 79
	lda #$7
	; Calling storevariable on generic assign expression
	sta scroll
	; LineNumber: 80
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; LineNumber: 81
	jsr screen_swap
	; LineNumber: 82
	lda #$1
	; Calling storevariable on generic assign expression
	sta ul
	jsr copy_colors
	; LineNumber: 83
	
; // color_shift_lower
	jsr DrawColumn39
	; LineNumber: 84
	rts
end_procedure_swap_screens
	; NodeProcedureDecl -1
	; ***********  Defining procedure : screen_swap
	;    Procedure type : User-defined procedure
	; LineNumber: 87
screen_swap
	; LineNumber: 89
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda screen_buffer_num
	clc
	adc #$1
	 ; end add / sub var with constant
	and #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta screen_buffer_num
	; LineNumber: 91
	; Binary clause Simplified: EQUALS
	clc
	; cmp #$00 ignored
	bne screen_swap_elsedoneblock28
screen_swap_ConditionalTrueBlock26: ;Main true block ;keep 
	; LineNumber: 92
	; LineNumber: 93
	
; // Flip between 0 and 1
	; Integer constant assigning
	ldy #$04
	lda #$00
	; Calling storevariable on generic assign expression
	sta screen_base
	sty screen_base+1
	; LineNumber: 94
	; Integer constant assigning
	ldy #$08
	; Calling storevariable on generic assign expression
	sta screen_backbuffer_base
	sty screen_backbuffer_base+1
	; LineNumber: 95
screen_swap_elsedoneblock28
	; LineNumber: 97
	; Binary clause Simplified: EQUALS
	lda screen_buffer_num
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne screen_swap_elsedoneblock34
screen_swap_ConditionalTrueBlock32: ;Main true block ;keep 
	; LineNumber: 98
	; LineNumber: 99
	; Integer constant assigning
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta screen_base
	sty screen_base+1
	; LineNumber: 100
	; Integer constant assigning
	ldy #$04
	; Calling storevariable on generic assign expression
	sta screen_backbuffer_base
	sty screen_backbuffer_base+1
	; LineNumber: 101
screen_swap_elsedoneblock34
	; LineNumber: 103
	; Assigning memory location
	; integer assignment NodeVar
	ldy screen_base+1 ; keep
	lda screen_base
	; Calling storevariable on generic assign expression
	sta $400
	; LineNumber: 104
	rts
end_procedure_screen_swap
	; NodeProcedureDecl -1
	; ***********  Defining procedure : copy_colors
	;    Procedure type : User-defined procedure
	; LineNumber: 107
	; LineNumber: 106
ul	dc.b	0
copy_colors_block37
copy_colors
	; LineNumber: 109
	; Binary clause Simplified: EQUALS
	clc
	lda ul
	; cmp #$00 ignored
	bne copy_colors_elseblock40
copy_colors_ConditionalTrueBlock39: ;Main true block ;keep 
	; LineNumber: 110
	; LineNumber: 111
	lda #$0
	; Calling storevariable on generic assign expression
	sta startrow
	; LineNumber: 112
	lda #$c
	; Calling storevariable on generic assign expression
	sta endrow
	; LineNumber: 114
	jmp copy_colors_elsedoneblock41
copy_colors_elseblock40
	; LineNumber: 115
	; LineNumber: 116
	lda #$d
	; Calling storevariable on generic assign expression
	sta startrow
	; LineNumber: 117
	lda #$18
	; Calling storevariable on generic assign expression
	sta endrow
	; LineNumber: 118
copy_colors_elsedoneblock41
	; LineNumber: 130
	lda startrow
	; Calling storevariable on generic assign expression
	sta row
copy_colors_forloop46
	; LineNumber: 121
	; LineNumber: 129
	lda #$0
	; Calling storevariable on generic assign expression
	sta col
copy_colors_forloop63
	; LineNumber: 123
	; LineNumber: 125
	
; // col, row
	; ----------
	; AddressTable address, xoffset, yoffset
	; yoffset is complex
	lda row
	asl ; *2
	tax
	lda caddr,x   ; Address of table lo
	ldy caddr+1,x   ; Address of table hi
	clc
	adc col
	bcc copy_colors_dtnooverflow71
	iny  ; overflow into high byte
copy_colors_dtnooverflow71
	sta colormemory
	sty colormemory+1
	; LineNumber: 126
	; Load pointer array
	ldy #$1
	lda (colormemory),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (colormemory),y
	; LineNumber: 127
	lda this_color
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$1
	sta (colormemory),y
	; LineNumber: 128
copy_colors_forloopcounter65
copy_colors_loopstart66
	; Compare is onpage
	; Test Inc dec D
	inc col
	lda #$27
	cmp col ;keep
	bne copy_colors_forloop63
copy_colors_loopdone72: ;keep
copy_colors_forloopend64
copy_colors_loopend67
	; LineNumber: 129
copy_colors_forloopcounter48
copy_colors_loopstart49
	; Compare is onpage
	; Test Inc dec D
	inc row
	lda endrow
	cmp row ;keep
	bcs copy_colors_forloop46
copy_colors_loopdone73: ;keep
copy_colors_forloopend47
copy_colors_loopend50
	; LineNumber: 131
	; Test Inc dec D
	inc this_color
	; LineNumber: 132
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda this_color
	; Compare with pure num / var optimization
	cmp #$b;keep
	bcc copy_colors_elsedoneblock77
copy_colors_ConditionalTrueBlock75: ;Main true block ;keep 
	; LineNumber: 131
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_color
copy_colors_elsedoneblock77
	; LineNumber: 134
	rts
end_procedure_copy_colors
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawColumn39
	;    Procedure type : User-defined procedure
	; LineNumber: 137
DrawColumn39
	; LineNumber: 140
	lda screen_base+1
	; Calling storevariable on generic assign expression
	sta val
	; LineNumber: 142
	
; // $0400 or $4400
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
	; LineNumber: 142
	; MoveTo optimization
	lda #$00
	sta screenmemory
	lda val
	clc
	adc #$00
	sta screenmemory+1
	; LineNumber: 151
	
; //addbreakpoint();
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
DrawColumn39_forloop81
	; LineNumber: 144
	; LineNumber: 145
	
; // ScreenMemory pointer to x, y, address high
	lda this_char
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$27
	sta (screenmemory),y
	; LineNumber: 146
	lda screenmemory
	clc
	adc #$28
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc DrawColumn39_WordAdd95
	inc screenmemory+1
DrawColumn39_WordAdd95
	; LineNumber: 147
	; Test Inc dec D
	inc this_char
	; LineNumber: 148
	; Binary clause Simplified: GREATEREQUAL
	lda this_char
	; Compare with pure num / var optimization
	cmp #$b;keep
	bcc DrawColumn39_elsedoneblock99
DrawColumn39_ConditionalTrueBlock97: ;Main true block ;keep 
	; LineNumber: 147
	lda #$1
	; Calling storevariable on generic assign expression
	sta this_char
DrawColumn39_elsedoneblock99
	; LineNumber: 150
DrawColumn39_forloopcounter83
DrawColumn39_loopstart84
	; Compare is onpage
	; Test Inc dec D
	inc row
	lda #$19
	cmp row ;keep
	bne DrawColumn39_forloop81
DrawColumn39_loopdone102: ;keep
DrawColumn39_forloopend82
DrawColumn39_loopend85
	; LineNumber: 152
	rts
end_procedure_DrawColumn39
block1
main_block_begin_
	; LineNumber: 172
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop103
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow104
	iny
MainProgram_dtnooverflow104
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop103
	; LineNumber: 174
	
; // $D800 color address, 40 characters per column, 25 rows
	; LineNumber: 175
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 176
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
	; LineNumber: 177
	jmp * ; loop like (�/%
	; LineNumber: 179
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

