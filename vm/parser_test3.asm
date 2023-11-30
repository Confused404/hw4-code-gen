	# $Id: parser_test3.asm,v 1.3 2023/09/17 02:14:32 leavens Exp $
	.text start
start:	BNE $8, $7, +4
	ADDI $0, $30, 100
	EXIT 
	.data 1024
	WORD hundred = 100
	WORD forty = 40
	WORD six = 6
	.stack 4096
	.end
