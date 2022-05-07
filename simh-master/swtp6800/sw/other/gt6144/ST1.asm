       ; Show Star Treck USS Enterprise
       ; Earth and stars on GT-6144 graphic terminal
       ; using its gorgeous 64 horizontal x 96 vertical B/W 
       ; pixels resolution

       ; use keys 1-8 (main text console must have the focus)
       ; to do/undo screen reversing/blanking

start  equ $0000
ineee  equ $e1ac	; input char

       org $0004
paradr fdb $800c	; gt6144 paralell port address

outch  ldx paradr
       staa 0,x 	; out byte in a-reg
       ldab #$37	; to gt6144
       stab 1,x
       ldab 0,x
       ldab #$3f
       stab 1,x
       rts
       nop
       
entry  ldaa #$3c	; main program entry
       staa $8007
       ldx paradr
       ldab #$ff	; send $ff to gt6144
       stab 0,x		; $ff=not used cmd
       ldab #$3f
       stab 1,x
       jsr erase
       jsr show
ovr    ldx start       
       inx
       stx start
print  ldx start
       ldaa 0,x
       cmpa #$ff
       beq exit
disp1  adda #$00
out2   jsr outch
       ldx start       
       inx
       stx start
       ldaa 0,x
       cmpa #$fe
       beq ovr
disp2  adda #$00
       bra out2
       rts
display adda #$40	; bit6=1 -> set pixel on
       staa disp1+1	; a-reg is x offset
       tba
       adda #$80		; bit7=1 -> write to video ram
       staa disp2+1	; b-reg is y offset
       stx start	; image start address
       jsr print
       rts
delay  ldaa #$00	; delay depending 
       ldx #$0000	; on b-reg value
ov     inx
       cpx #$ffff
       beq inc
       bra ov
inc    inca
       cba
       beq exit
       bra ov
exit   rts                     

erase  ldaa hpos	; clear screen
       cmpa #$40	; then brach to out
       beq out1		; routine
       jsr outch
increm ldaa vpos
       cmpa #$60
       beq spec
       adda #$80
       jsr outch
       inc vpos
       bra increm
spec   ldaa #$00
       staa vpos
       inc hpos
       bra erase
out1   ldaa #$00	; reset vpos and hpos
       staa hpos	; then return
       staa vpos
       rts    
show   ldaa #$05
       ldab #$0d
       ldx #img1
       jsr display
       ldaa #$1b
       ldab #$40
       ldx #img2
       jsr display            
       ldab #$0a
       jsr delay
       ldaa #$00
       ldab #$00
       ldx #img3
       jsr display            
       jsr over            
over   jsr ineee 	; input char
       cmpa #'1'	; options allowed
       beq one		; 1 to 8
       cmpa #'2'
       beq two
       cmpa #'3'
       beq three
       cmpa #'4'
       beq four
       cmpa #'5'
       beq five
       cmpa #'6'
       beq six
       cmpa #'7'
       beq seven
       cmpa #'8'
       beq eight
       bra over
one    ldaa #$e0	; invert screen
       bra out       
two    ldaa #$e1	; normal screen
       bra out       
three  ldaa #$e2	; disable ct-1024
       bra out       
four   ldaa #$e3	; enable ct-1024
       bra out       
five   ldaa #$e4	; enable graphics
       bra out       
six    ldaa #$e5	; blank graphic scren 
       bra out       
seven  ldaa #$e6	; not used
       bra out       
eight  ldaa #$e7	; not used
out    jsr outch
       bra over
       
hpos   rmb 1
vpos   rmb 1       

       org $0200
       
img1   fcb $00, $0A, $0B, $0C, $FE, $01 
       fcb $0A, $0C, $FE, $02, $0A, $0C
       fcb $FE, $03, $09, $0C, $FE, $04
       fcb $09, $0C, $FE, $05, $09, $0C
       fcb $FE, $06, $08, $0C, $FE, $07 
       fcb $08, $0C, $FE, $08, $08, $0D
       fcb $FE, $09, $07, $0E, $FE, $0A 
       fcb $05, $0F, $FE, $0B, $04, $05 
       fcb $0F, $10, $FE, $0C, $04, $05 
       fcb $0F, $10, $FE, $0D, $05, $0F 
       fcb $FE, $0E, $05, $0E, $FE, $0F 
       fcb $06, $0D, $FE, $10, $07, $0C 
       fcb $FE, $11, $07, $0C, $19, $FE
       fcb $12, $08, $0C, $0D, $16, $17
       fcb $18, $19, $1A, $1B, $1C, $FE 
       fcb $13, $08, $0C, $0F, $17, $18 
       fcb $19, $1A, $1B, $FE, $14, $08 
       fcb $0C, $10, $16, $17, $18, $19 
       fcb $1A, $1B, $1C, $FE, $15, $08
       fcb $0C, $11, $15, $1D, $FE, $16 
       fcb $08, $0C, $12, $14, $19, $1E 
       fcb $FE, $17, $08, $0C, $13, $19 
       fcb $1E, $FE, $18, $08, $0C, $13 
       fcb $19, $1F, $FE, $19, $09, $0A 
       fcb $0B, $0D, $13, $19, $1F, $FE 
       fcb $1A, $05, $0F, $13, $1F, $FE 
       fcb $1B, $03, $04, $06, $07, $11
       fcb $14, $19, $1F, $FE, $1C, $02 
       fcb $08, $13, $14, $19, $1F, $FE 
       fcb $1D, $02, $08, $14, $19, $1E 
       fcb $FE, $1E, $02, $08, $14, $1E 
       fcb $FE, $1F, $02, $08, $14, $1E 
       fcb $FE, $20, $02, $08, $14, $1E 
       fcb $FE, $21, $02, $08, $09, $0A 
       fcb $0B, $0C, $0D, $0E, $0F, $10
       fcb $11, $12, $13, $14, $15, $1E 
       fcb $FE, $22, $02, $1D, $FE, $23 
       fcb $02, $1D, $FE, $24, $02, $08 
       fcb $09, $0A, $0B, $0C, $0D, $0E 
       fcb $0F, $10, $11, $12, $13, $14 
       fcb $15, $1D, $FE, $25, $02, $08 
       fcb $15, $1D, $FE, $26, $02, $08 
       fcb $15, $1B, $FE, $27, $02, $08
       fcb $15, $1A, $FE, $28, $02, $08 
       fcb $15, $1A, $FE, $29, $02, $08 
       fcb $15, $1A, $FE, $2A, $02, $08 
       fcb $16, $17, $18, $19, $1A, $FE 
       fcb $2B, $02, $08, $FE, $2C, $02 
       fcb $08, $FE, $2D, $02, $08, $FE 
       fcb $2E, $02, $08, $FE, $2F, $02 
       fcb $08, $FE, $30, $01, $02, $08
       fcb $FE, $31, $01, $02, $08, $FE 
       fcb $32, $01, $02, $08, $FE, $33 
       fcb $01, $02, $08, $FE, $34, $01 
       fcb $02, $08, $FE, $35, $01, $02 
       fcb $08, $08, $FE, $36, $01, $02 
       fcb $08, $FE, $37, $02, $08, $FE 
       fcb $38, $02, $04, $05, $06, $07 
       fcb $FE, $39, $02, $03, $FE, $2A
       fcb $15, $FE, $FF

img2   fcb $C0, $1A, $1D, $1F, $FE, $01 
       fcb $17, $FE, $02, $13, $FE, $03 
       fcb $10, $FE, $04, $0D, $FE, $05 
       fcb $0B, $FE, $06, $0A, $FE, $07 
       fcb $08, $FE, $08, $07, $FE, $09 
       fcb $06, $FE, $0A, $05, $06, $07 
       fcb $FE, $0B, $04, $05, $06, $07 
       fcb $0B, $FE, $0C, $04, $05, $06 
       fcb $07, $0A, $FE, $0D, $03, $06 
       fcb $09, $0A, $FE, $0E, $07, $08 
       fcb $09, $FE, $0F, $02, $08, $09 
       fcb $0A, $0B, $0C, $0D, $0F, $10 
       fcb $FE, $10, $09, $0A, $0B, $0C 
       fcb $0D, $0E, $0F, $10, $11, $12 
       fcb $13, $FE, $11, $02, $09, $0A 
       fcb $0B, $0C, $0D, $0E, $0F, $10 
       fcb $11, $12, $13, $14, $15, $FE 
       fcb $12, $02, $05, $07, $0B, $0C 
       fcb $0D, $0E, $0F, $10, $11, $12 
       fcb $13, $14, $15, $16, $17, $FE 
       fcb $13, $04, $05, $06, $08, $0C 
       fcb $0D, $0E, $0F, $10, $11, $12 
       fcb $13, $17, $FE, $14, $02, $06 
       fcb $07, $0A, $0B, $0C, $0D, $0E 
       fcb $0F, $10, $11, $12, $13, $14 
       fcb $17, $FE, $15, $07, $0B, $0C 
       fcb $0D, $0E, $16, $18, $1C, $FE 
       fcb $16, $03, $18, $19, $1A, $1B 
       fcb $1C, $1D, $1E, $1F, $1F, $FE 
       fcb $17, $04, $16, $19, $1A, $1B 
       fcb $1C, $1D, $1E, $1F, $1F, $FE 
       fcb $18, $05, $19, $1A, $1B, $1C 
       fcb $1D, $1E, $1F, $1F, $FE, $19 
       fcb $06, $1C, $1D, $1E, $1F, $1F 
       fcb $FE, $1A, $07, $1D, $1E, $1F 
       fcb $1F, $FE, $1B, $08, $1E, $FE 
       fcb $1C, $0A, $FE, $1D, $0B, $FE 
       fcb $1E, $0D, $FE, $1F, $10, $FE 
       fcb $20, $13, $FE, $21, $17, $FE 
       fcb $22, $1A, $1D, $1F, $FE, $FF

img3   fcb $01, $2C, $FE, $02, $58, $FE 
       fcb $05, $04, $FE, $0A, $1E, $FE 
       fcb $0C, $40, $FE, $0D, $30, $FE 
       fcb $0F, $44, $FE, $14, $58, $FE 
       fcb $19, $4C, $FE, $1E, $3A, $FE 
       fcb $20, $06, $FE, $26, $30, $FE 
       fcb $31, $0A, $FE, $34, $34, $FE 
       fcb $35, $1C, $FE, $3B, $02, $2A 
       fcb $FE, $3E, $42, $FE, $FF
       
       end