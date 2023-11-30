/* $Id: lexer.c,v 1.11 2023/10/06 15:28:52 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include "ast.h"
#include "parser_types.h"
#include "lexer.h"
#include "machine_types.h"
#include "utilities.h"


// All of the other lexer_* functions declared in lexer.h
// that are not defined here
// are defined in asm_lexer.l's user code section.


// Requires: toknum is a token number (from asm.tab.h)
//           of an instruction
// Return the opcode corresponding to the given opcode token number
op_code lexer_token2opcode(int toknum)
{
    op_code ret;

    switch (toknum) {
    // register type op codes
    case addopsym: case subopsym: case mulopsym: case divopsym:
    case mfhiopsym: case mfloopsym:
    case andopsym: case boropsym: case noropsym: case xoropsym:
    case sllopsym: case srlopsym: case jropsym:
	ret = REG_O; // distinguished by func field
	break;
    // system call op codes
    case exitopsym: case pstropsym: case pchopsym:
    case rchopsym: case straopsym: case notropsym:
	ret = REG_O;  // opcode is REG_O, func SYSCALL_F, distinuished by code
	break;
    // immedidate format op codes
    case addiopsym:
	ret = ADDI_O;
	break;
    case andiopsym:
	ret = ANDI_O;
	break;
    case boriopsym:
	ret = BORI_O;
	break;
    case xoriopsym:
	ret = XORI_O;
	break;
    case beqopsym:
	ret = BEQ_O;
	break;
    case bgezopsym:
	ret = BGEZ_O;
	break;
    case bgtzopsym:
	ret = BGTZ_O;
	break;
    case blezopsym:
	ret = BLEZ_O;
	break;
    case bltzopsym:
	ret = BLTZ_O;
	break;
    case bneopsym:
	ret = BNE_O;
	break;
    case lbuopsym:
	ret = LBU_O;
	break;
    case lwopsym:
	ret = LW_O;
	break;
    case sbopsym:
	ret = SB_O;
	break;
    case swopsym:
	ret = SW_O;
	break;
	// jump format instructions
    case jmpopsym:
	ret = JMP_O;
	break;
    case jalopsym:
	ret = JAL_O;
	break;
    default:
	bail_with_error("Token number not of an op code (%d) in lexer_token2opcode!",
			toknum);
	break;
    }
    return ret;
}

// Requires: toknum is a token number (from asm.tab.h)
//           that corresponds to a register instruction
// Return the function code corresponding to the given opcode token number
func_code lexer_token2func(int toknum)
{
    func_code ret;

    switch (toknum) {
    case addopsym:
	ret = ADD_F;
	break;
    case subopsym:
	ret = SUB_F;
	break;
    case andopsym:
	ret = AND_F;
	break;
    case boropsym:
	ret = BOR_F;
	break;
    case noropsym:
	ret = NOR_F;
	break;
    case xoropsym:
	ret = XOR_F;
	break;
    case mulopsym:
	ret = MUL_F;
	break;
    case divopsym:
	ret = DIV_F;
	break;
    case sllopsym:
	ret = SLL_F;
	break;
    case srlopsym:
	ret = SRL_F;
	break;
    case mfhiopsym:
	ret = MFHI_F;
	break;
    case mfloopsym:
	ret = MFLO_F;
	break;
    case jropsym:
	ret = JR_F;
	break;
    case exitopsym: case pstropsym: case pchopsym:
    case rchopsym: case straopsym: case notropsym:
	ret = SYSCALL_F;
	break;
    default:
	bail_with_error("Unhandled token (%d) in lexer_token2func", toknum);
	break;
    }
    return ret;
}

// Requires: toknum is a token number (from asm.tab.h)
//           that corresponds to a system call instruction
// Return the system call code that corresponds to that token
syscall_type lexer_token2syscall_code(int toknum)
{
    syscall_type ret;
    switch (toknum) {
    case exitopsym:
	ret = exit_sc;
	break;
    case pstropsym:
	ret = print_str_sc;
	break;
    case pchopsym:
	ret = print_char_sc;
	break;
    case rchopsym:
	ret = read_char_sc;
	break;
    case straopsym:
	ret = start_tracing_sc;
	break;
    case notropsym:
	ret = stop_tracing_sc;
	break;
    default:
	bail_with_error("Unknown token (%d) given to lexer_token2syscall_code!",
			toknum);
	ret = 0;
    }
    return ret;
}
