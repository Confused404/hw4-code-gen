	# $Id: lexer_test6.asm,v 1.2 2023/09/16 13:29:53 leavens Exp $
	.text start
start:	ADD $at, $at, $at
	EXIT
	.data 1024
	.stack 4096
	.end
	
