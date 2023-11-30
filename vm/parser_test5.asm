  # $Id: parser_test5.asm,v 1.5 2023/09/18 02:24:31 leavens Exp $

.text here

start:	ADD $0, $2, $3
        SUB $3, $2, $5
	AND $4, $5, $4
	BOR $6, $7, $8 
	NOR $9, $10, $11  # why?
	XOR $12, $13, $14 # & why not?
        MUL $15, $16
	MFHI $17
	MFLO $18
	DIV $18, $17
	ADDI $0, $13, 13
	ADD $0, $0, $31
	JR $13
here:	ANDI $7, $7, 8
	XORI $8, $7, 0x34567A
	BEQ $8, $7, -0x10
        BNE $8, $7, +4
	BGEZ $8, 4
	BGTZ $9, +8
	BLTZ $10, 4
	BLEZ $9, 4
	ADDI $0, $30, 100  # put 100 in fp
	ADDI $0, $9, 10    # put 10 in $9
	SB   $30, $9, 5
	LBU  $30, $8, 5
        SW   $30, $8, 0x8
	ADD  $0,  $30, $4
	PSTR
	PCH
	RCH
        EXIT
       
.data   2048
	WORD b = 10
	WORD w
	WORD c
	WORD sixteen = 0x10

.stack  6144
.end
