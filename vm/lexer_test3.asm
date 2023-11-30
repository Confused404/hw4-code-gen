	# $Id: lexer_test3.asm,v 1.2 2023/09/16 13:29:53 leavens Exp $
	.text go
go:     ADDI $0, $9, 52
        ADDI $0, $10, -0934
	EXIT
	.data 512
	WORD sixteen = 0x10
	.stack 4512
	.end
	
