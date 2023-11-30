# $Id: parser_test6.asm,v 1.2 2023/09/18 02:24:31 leavens Exp $
.text start
zero:	ADD $0, $v0, $v1
	SUB $v1, $v0, $a1
	AND $a0, $a1, $a0
	BOR $a2, $a3, $t0
	NOR $t1, $t2, $t3	# why?
	XOR $t4, $t5, $t6 # & why not?
	MUL $t7, $s0
	MFHI $s1
	MFLO $s2
	DIV $s2, $s1
start:	ADDI $0, $t5, 13
	ADD $0, $0, $ra
	JR $0
	ANDI $a3, $a3, 0x8
	XORI $t0, $a3, 0x567a
	BEQ $t0, $a3, -16
	BNE $t0, $a3, 4
	BGEZ $t0, 4
	BGTZ $t1, 8
	BLTZ $t2, 4
	BLEZ $t1, 4
	ADDI $0, $fp, 100   # put 100 in fp
	ADDI $0, $t1, 10   # put 10 in $9
	SB $fp, $t1, 5
	LBU $fp, $t0, 5
	SW $fp, $t0, 8
	ADD $0, $fp, $a0
	PSTR 
	PCH 
	RCH 
	STRA
	NOTR
	EXIT 
.data 2048
WORD w0 = 10
WORD w1 = 0
WORD w2 = -272345
WORD w3 = 16
.stack 6144
.end
