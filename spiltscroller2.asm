
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
	; LineNumber: 69
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
scroll	dc.b	$07
	; NodeProcedureDecl -1
	; ***********  Defining procedure : start_colorcopy
	;    Procedure type : User-defined procedure
	; LineNumber: 30
start_colorcopy
	; LineNumber: 32
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 34
	
; // do upper colour shift
	; Binary clause Simplified: EQUALS
	clc
	lda scroll
	; cmp #$00 ignored
	bne start_colorcopy_elsedoneblock6
start_colorcopy_ConditionalTrueBlock4: ;Main true block ;keep 
	; LineNumber: 35
	; LineNumber: 37
start_colorcopy_elsedoneblock6
	; LineNumber: 39
	; RasterIRQ : Hook a procedure
	lda #$f5
	sta $d012
	lda #<begin_vblank
	sta $fffe
	lda #>begin_vblank
	sta $ffff
	; LineNumber: 41
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 43
	rti
end_procedure_start_colorcopy
	
; // vblank line starts at 245
	; NodeProcedureDecl -1
	; ***********  Defining procedure : begin_vblank
	;    Procedure type : User-defined procedure
	; LineNumber: 47
begin_vblank
	; LineNumber: 49
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; LineNumber: 51
	; Test Inc dec D
	dec scroll
	; LineNumber: 53
	
; //swap screens
	; Optimization: replacing a <= N with a <= N-1
	; Binary clause Simplified: LESS
	lda scroll
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcs begin_vblank_elsedoneblock13
begin_vblank_ConditionalTrueBlock11: ;Main true block ;keep 
	; LineNumber: 54
	; LineNumber: 56
begin_vblank_elsedoneblock13
	; LineNumber: 58
	lda scroll
	; ScrollX method
	sta  $58
	lda $d016  
	and #$F8
	ora  $58
	sta $d016
	; LineNumber: 60
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 62
	rti
end_procedure_begin_vblank
block1
main_block_begin_
	; LineNumber: 71
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	; LineNumber: 72
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; RasterIRQ : Hook a procedure
	lda #$41
	sta $d012
	lda #<start_colorcopy
	sta $fffe
	lda #>start_colorcopy
	sta $ffff
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	; LineNumber: 73
	jmp * ; loop like (�/%
	; LineNumber: 75
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

