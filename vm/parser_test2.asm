	# $Id: parser_test2.asm,v 1.2 2023/09/16 13:29:55 leavens Exp $
	.text start
start:	ADDI $0, $30, 100
	EXIT
	.data 104
	.stack 4200
	.end
