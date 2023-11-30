/* $Id: machine.c,v 1.19 2023/11/14 18:29:49 leavens Exp leavens $ */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "machine_types.h"
#include "machine.h"
#include "regname.h"
#include "utilities.h"

#define MAX_PRINT_WIDTH 59

// the VM's memory, both in byte, word, and binary instruction views.
static union mem_u {
    byte_type bytes[MEMORY_SIZE_IN_BYTES];
    word_type words[MEMORY_SIZE_IN_WORDS];
    bin_instr_t instrs[MEMORY_SIZE_IN_WORDS];
} memory;

// general purpose registers
static word_type GPR[NUM_REGISTERS];
// hi and lo registers used in multiplication and division,
// and a view as a long
static union longAs2words_u {
    unsigned long result;
    word_type hilo[2]; // lo is index 0, hi is index 1
                       // because the x86 is little-endian
} hilo_regs;

#define LO 0
#define HI 1

// the program counter
static address_type PC;

// should the machine be printing tracing output?
static bool tracing;

// words of instructions loaded (based on the header)
static unsigned int instructions_loaded;
// words of global data (based on the header)
static unsigned int global_data_words;

// the bottom of the stack from the BOF file, for tracing purposes
static unsigned int stack_bottom_address;

// should the machine be running? (default true)
static bool running;

// set up the state of the machine
static void initialize()
{
    tracing = false;   // default for tracing
    instructions_loaded = 0;
    global_data_words = 0;
    running = true;

    // zero the registers
    for (int j = 0; j < NUM_REGISTERS; j++) {
	GPR[j] = 0;
    }
    hilo_regs.result = 0;
    // zero out the memory
    for (int i = 0; i < MEMORY_SIZE_IN_WORDS; i++) {
	memory.words[i] = 0;
    }
}

// Requires: bf is a binary object file that is open for reading
// Load count instructions in bf into the memory starting at address 0.
// If any errors are encountered, exit with an error message.
static void load_instructions(BOFFILE bf, int count)
{
    for (int i = 0; i < count; i++) {
	memory.instrs[i] = instruction_read(bf);
    }
}

// Requires: bf is a binary object file that is open for reading
// Load count words in bf into the memory starting at address global_base,
// which is a word address.
// If any errors are encountered, exit with an error message.
static void load_data(BOFFILE bf, int count, unsigned int global_base)
{
    for (int i = 0; i < count; i++) {
	memory.words[global_base+i] = bof_read_word(bf);
    }
}

// Requires: bf is open for reading in binary
// Load the binary object file bf, and get ready to run it
void machine_load(BOFFILE bf)
{
    initialize();

    // read and check the header
    BOFHeader bh = bof_read_header(bf);
    if (bh.text_start_address % BYTES_PER_WORD != 0) {
	bail_with_error("PC starting address (%u) is not divisible by %d!",
			bh.text_start_address, BYTES_PER_WORD);
    }
    if (bh.text_length >= bh.data_start_address) {
	bail_with_error("%s (%u) %s (%u)!",
			"Text, i.e., program length", bh.text_length,
			"is not less than the start address of the global data",
			bh.data_start_address);
    }
    if (bh.data_start_address % BYTES_PER_WORD != 0) {
	bail_with_error("Data start address (%u) is not divisible by %d",
			bh.data_start_address, BYTES_PER_WORD);
    }
    if (bh.data_start_address + bh.data_length >= bh.stack_bottom_addr) {
	bail_with_error("%s (%u) + %s (%u) %s (%u)!",
			"Global data start address", bh.data_start_address,
			"global data length", bh.data_length,
			"is not less than the stack bottom address",
			bh.stack_bottom_addr);
    }
    if (bh.stack_bottom_addr % BYTES_PER_WORD != 0) {
	bail_with_error("Stack bottom address (%u) is not divisible by %d",
			bh.stack_bottom_addr, BYTES_PER_WORD);
    }
    if (bh.stack_bottom_addr >= MEMORY_SIZE_IN_BYTES) {
	bail_with_error("%s (%u) %s (%u)!",
			"stack_bottom_addr", bh.stack_bottom_addr,
			"is not less than the memory size",
			MEMORY_SIZE_IN_BYTES);
    }

    // load the program
    instructions_loaded = bh.text_length / BYTES_PER_WORD;
    load_instructions(bf, instructions_loaded);

    global_data_words = bh.data_length / BYTES_PER_WORD;
    
    load_data(bf, global_data_words,
	      bh.data_start_address / BYTES_PER_WORD);

    // initialize the registers
    PC = bh.text_start_address;

    // save the address of the stack bottom, as specified in the BOF file
    stack_bottom_address = bh.stack_bottom_addr;

    GPR[GP] = bh.data_start_address;
    GPR[SP] = stack_bottom_address;
    GPR[FP] = stack_bottom_address;
    // to simulate a call, put the stack bottom address in a0
    GPR[A0] = stack_bottom_address;
}

// print the memory location at word address a to out
// (using the byte address, whic is WORDS_PER_BYTE times a)
// with a format determined by fmt and no newline,
// returns the number of characters written
static int print_loc(FILE *out, int a, char fmt)
{
    int count;
    if (fmt == 'x') {
	count = fprintf(out, "%8d: 0x%x\t", a*BYTES_PER_WORD, memory.words[a]);
    } else {
	count = fprintf(out, "%8d: %d\t", a*BYTES_PER_WORD, memory.words[a]);
    }
    return count;
}

// print the address given and 
static void print_instruction(FILE *out, address_type a, bin_instr_t bi)
{
    fprintf(out, "%4d %s\n", a, instruction_assembly_form(bi));
}

// print the word memory in hex or decimal based on the fmt argument
// between start (inclusive) and end (inclusive) to out,
// without a newline and eliding most elements that are 0
static bool print_memory_nonzero(FILE *out, int start, int end, char fmt)
{
    bool printed_trailing_newline = false;
    bool no_dots_yet = true;
    // count of chars on a line
    int lc = 0;
    for (int a = start; a <= end; a++) {
	if (memory.words[a] != 0) {
	    lc += print_loc(out, a, fmt);
	    printed_trailing_newline = false;
	    no_dots_yet = true;
	} else { // memory.words[a] == 0
	    if (no_dots_yet) {
		lc += print_loc(out, a, fmt);
		lc += fprintf(out, "...");
		printed_trailing_newline = false;
		no_dots_yet = false;
	    }
	}
	if (lc > MAX_PRINT_WIDTH) {
	    newline(out);
	    printed_trailing_newline = true;
	    lc = 0;
	}
    }
    return printed_trailing_newline;
}

/*
// print the nonzero memory locations between start and end on out,
// in hexadecimal notation
// a trailing newline was printed if the result is true
static bool print_memory_words_x(FILE *out, int start, int end)
{
    return print_memory_nonzero(out, start, end, 'x');
}
*/

// print the nonzero memory locations between start and end on out,
// in decimal notation
// a trailing newline was printed if the result is true
static bool print_memory_words_d(FILE *out, int start, int end)
{
    return print_memory_nonzero(out, start, end, 'd');
}

// Print the non-zero global data from GPR[GP] for global_data_words
static void print_global_data(FILE *out)
{
    int global_wa = GPR[GP] / BYTES_PER_WORD;
    bool printed_nl = print_memory_words_d(out, global_wa,
					   global_wa + global_data_words);
    if (!printed_nl) {
	newline(out);
    }
}

// Requires: a program has been loaded into the computer's memory
// print a heading and the program and any global data
// that were previously loaded into the VM's memory to out
void machine_print_loaded_program(FILE *out)
{
    // heading
    instruction_print_table_heading(out);
    // instructions
    for (int wa = 0; wa < instructions_loaded; wa++) {
	print_instruction(out, wa*BYTES_PER_WORD, memory.instrs[wa]);
    }

    print_global_data(out);
}

// Run the VM on the already loaded program,
// producing any trace output called for by the program
void machine_run(bool should_trace)
{
    tracing = should_trace;
    
    if (tracing) {
	machine_print_state(stdout);
    }
    // execute the program
    while (running) {
	machine_okay(); // check the invariant
	machine_trace_execute_instr(stdout,
				    memory.instrs[PC/BYTES_PER_WORD]);
    }
}

// Load the given binary object file and run it,
// tracing if should_trace is true
void machine_load_and_run(BOFFILE bf, bool should_trace)
{
    machine_load(bf);
    machine_run(should_trace);
}

// If tracing then print bi, execute bi (always),
// then if tracing print out the machine's state.
// All tracing output goes to the FILE out
void machine_trace_execute_instr(FILE *out, bin_instr_t bi)
{
    if (tracing) {
	fprintf(out, "==> addr: ");
	print_instruction(out, PC, bi);
    }
    machine_execute_instr(bi);
    if (tracing) {
	machine_print_state(out);
    }
}

// Execute the given instruction, in the machine's current state
void machine_execute_instr(bin_instr_t bi)
{
    // first, increment the PC
    PC = PC + BYTES_PER_WORD;
    
    instr_type it = instruction_type(bi);
    switch (it) {
    case reg_instr_type:
	{
	    reg_instr_t ri = bi.reg;
	    switch (ri.func) {
	    case ADD_F:
		GPR[ri.rd] = GPR[ri.rs] + GPR[ri.rt];
		break;
	    case SUB_F:
		GPR[ri.rd] = GPR[ri.rs] - GPR[ri.rt];
		break;
	    case MUL_F:
		hilo_regs.result = GPR[ri.rs] * GPR[ri.rt];
		break;
	    case DIV_F:
		if (GPR[ri.rt] == 0) {
		    bail_with_error("Attempt to divide by zero!");
		}
		hilo_regs.hilo[HI] = GPR[ri.rs] % GPR[ri.rt];
		hilo_regs.hilo[LO] = GPR[ri.rs] / GPR[ri.rt];
		break;
	    case MFHI_F:
		GPR[ri.rd] = hilo_regs.hilo[HI];
		break;
	    case MFLO_F:
		GPR[ri.rd] = hilo_regs.hilo[LO];
		break;
	    case AND_F:
		GPR[ri.rd] = GPR[ri.rs] & GPR[ri.rt];
		break;
	    case BOR_F:
		GPR[ri.rd] = GPR[ri.rs] | GPR[ri.rt];
		break;
	    case NOR_F:
		GPR[ri.rd] = ~(GPR[ri.rs] | GPR[ri.rt]);
		break;
	    case XOR_F:
		GPR[ri.rd] = GPR[ri.rs] ^ GPR[ri.rt];
		break;
	    case SLL_F:
		GPR[ri.rd] = GPR[ri.rt] << ri.shift;
		break;
	    case SRL_F:
		GPR[ri.rd] = ((unsigned int)GPR[ri.rt]) >> ri.shift;
		break;	
	    case JR_F:
		PC = GPR[ri.rs];
		break;
	    default:
		bail_with_error("Invalid function code (%d) in machine_execute's register instruction case!",
				ri.func);
		break;
	    }
	}
	break;
    case syscall_instr_type:
	switch (instruction_syscall_number(bi)) {
	case exit_sc:
	    running = false;
	    exit(0);
	    break;
	case print_str_sc:
	    GPR[V0] = printf("%s", &memory.bytes[GPR[A0]]);
	    break;
	case print_char_sc:
	    GPR[V0] = fputc(GPR[A0], stdout);
	    break;
	case print_int_sc:
	    GPR[V0] = printf("%d", GPR[A0]);
	    break;
	case read_char_sc:
	    GPR[V0] = getc(stdin);
	    break;
	case start_tracing_sc:
	    tracing = true;
	    break;
	case stop_tracing_sc:
	    tracing = false;
	    break;
	default:
	    bail_with_error("Invalid system call type (%d) in machine_execute's syscall instruction case!",
			    instruction_syscall_number(bi));
	}
	break;
    case immed_instr_type:
	{
	    immed_instr_t ii = bi.immed;
	    switch (ii.op) {
	    case ADDI_O:
		GPR[ii.rt] = GPR[ii.rs] + machine_types_sgnExt(ii.immed);
		break;
	    case ANDI_O:
		GPR[ii.rt] = GPR[ii.rs] & machine_types_zeroExt(ii.immed);
		break;
	    case BORI_O:
		GPR[ii.rt] = GPR[ii.rs] | machine_types_zeroExt(ii.immed);
		break;
	    case XORI_O:
		GPR[ii.rt] = GPR[ii.rs] ^ machine_types_zeroExt(ii.immed);
		break;
	    case BEQ_O:
		if (GPR[ii.rs] == GPR[ii.rt]) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case BGEZ_O:
		if (GPR[ii.rs] >= 0) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case BGTZ_O:
		if (GPR[ii.rs] > 0) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case BLEZ_O:
		if (GPR[ii.rs] <= 0) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case BLTZ_O:
		if (GPR[ii.rs] < 0) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case BNE_O:
		if (GPR[ii.rs] != GPR[ii.rt]) {
		    PC = PC + machine_types_formOffset(ii.immed);
		}
		break;
	    case LBU_O:
		{
		    address_type ba = GPR[ii.rs] + machine_types_formOffset(ii.immed);
		    GPR[ii.rt] = machine_types_zeroExt(memory.bytes[ba]);
		}
		break;
	    case LW_O:
		{
		    address_type wa = (GPR[ii.rs] + machine_types_formOffset(ii.immed))
			/ BYTES_PER_WORD;
		    GPR[ii.rt] = memory.words[wa];
		}
		break;
	    case SB_O:
		{
		    address_type ba = GPR[ii.rs] + machine_types_formOffset(ii.immed);
		    memory.bytes[ba] = GPR[ii.rt];
		}
		break;
	    case SW_O:
		{
		    address_type wa = (GPR[ii.rs] + machine_types_formOffset(ii.immed))
			/ BYTES_PER_WORD;
		    memory.words[wa] = GPR[ii.rt];
		}
		break;
	    default:
		bail_with_error("Invalid opcode (%d) in machine_execute's immediate instruction case!",
				ii.op);	    
		break;
	    }
	}
	break;
    case jump_instr_type:
	{
	    jump_instr_t ji = bi.jump;
	    switch (ji.op) {
	    case JMP_O:
		PC = machine_types_formAddress(PC, ji.addr);
		break;
	    case JAL_O:
		GPR[RA] = PC;
		PC = machine_types_formAddress(PC, ji.addr);
		break;
	    default:
		bail_with_error("Invalid opcode (%d) in machine_execute's jump instruction case!",
				ji.op);	    
		break;
	    }
	}
	break;
    default:
	bail_with_error("Invalid instruction type (%d) in machine_execute!",
			it);
	break;
    }
}

#define    REGFORMAT1 "GPR[%-3s]: %-4d"
#define    REGFORMAT2 "\tGPR[%-3s]: %-4d"

// Requires: out != NULL and out can be written on
// Print the current values in the registers to out
static void print_registers(FILE *out)
{
    // print the registers
    fprintf(out, "%8s: %u", "PC", PC);
    if (hilo_regs.result != 0L) {
	fprintf(out, "\t%8s: %d\t%8s: %d", "HI", hilo_regs.hilo[HI], "LO", hilo_regs.hilo[LO]);
    }
    newline(out);
    int j;

    for (j = 0; j < (NUM_REGISTERS - 6); /* nothing */) {
	fprintf(out, REGFORMAT1, regname_get(j), GPR[j]);
	j++;
	for (int i = 0; i < 5; i++) {
	    fprintf(out, REGFORMAT2, regname_get(j), GPR[j]);
	    j++;
	}
	newline(out);
    }
    fprintf(out, REGFORMAT1, regname_get(j), GPR[j]);
    j++;
    fprintf(out, REGFORMAT2, regname_get(j), GPR[j]);
    j++;
    newline(out);
}


// Print non-zero global data between GPR[SP] and stack_bottom_address
static void print_runtime_stack_AR(FILE *out)
{
    // print the memory between sp and stack_bottom_address, inclusive
    bool printed_nl
	= print_memory_words_d(out,
			       GPR[SP] / BYTES_PER_WORD,
			       (stack_bottom_address / BYTES_PER_WORD)+1);
    if (!printed_nl) {
	newline(out);
    }
}

// Requires: out != NULL and out can be written on
// print the state of the machine (registers, globals, and
// the memory between GPR[$sp] and GPR[$fp], inclusive) to out
void machine_print_state(FILE *out)
{
    print_registers(out);
    print_global_data(out);
    print_runtime_stack_AR(out);
}

// Invariant test for the VM (for debugging purposes)
// This exits with an assertion error if the invariant does not pass
void machine_okay()
{
    assert(PC % BYTES_PER_WORD == 0);
    assert(GPR[GP] % BYTES_PER_WORD == 0);
    assert(GPR[SP] % BYTES_PER_WORD == 0);
    assert(GPR[FP] % BYTES_PER_WORD == 0);
    assert(0 <= GPR[GP]);
    assert(GPR[GP] < GPR[SP]);
    assert(GPR[SP] <= GPR[FP]);
    assert(GPR[FP] < MEMORY_SIZE_IN_BYTES);
    assert(GPR[0] == 0);
}
