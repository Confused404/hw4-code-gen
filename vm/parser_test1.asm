	# $Id: parser_test1.asm,v 1.3 2023/09/16 16:23:10 leavens Exp $

.text begin
begin:	SUB $3, $2, $5
        EXIT
.data   0x40
	WORD b = 10
	WORD w
	WORD c
	WORD sixteen = 16
.stack  0x4040
.end
