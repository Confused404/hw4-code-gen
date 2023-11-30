	# $Id: parser_test4.asm,v 1.3 2023/09/16 16:23:10 leavens Exp $
	.text start
start:	BNE $0, $0, +4
	EXIT
	.data 2048
	WORD five = +5
	.stack 6144
	.end
