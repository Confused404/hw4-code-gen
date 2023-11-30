/* $Id: machine.h,v 1.8 2023/11/14 18:29:49 leavens Exp leavens $ */
#ifndef _MACHINE_H
#define _MACHINE_H
#include <assert.h>
#include "machine_types.h"
#include "bof.h"
#include "instruction.h"

// a size for the memory (2^16 bytes = 64K)
#define MEMORY_SIZE_IN_BYTES (65536 - BYTES_PER_WORD)
#define MEMORY_SIZE_IN_WORDS (MEMORY_SIZE_IN_BYTES / BYTES_PER_WORD)

// Requires: bf is open for reading in binary
// Load the binary object file bf, and get ready to run it
extern void machine_load(BOFFILE bf);

// Requires: a program has been loaded into the computer's memory
// print a heading and the program in the VM's memory to out
extern void machine_print_loaded_program(FILE *out);

// Run the VM on the already loaded program,
// producing trace output by default if should_trace is true
extern void machine_run(bool should_trace);

// Load the given binary object file and run it,
// tracing if should_trace is true
extern void machine_load_and_run(BOFFILE bf, bool should_trace);

// If tracing then print bi, execute bi (always),
// then if tracing print out the machine's state.
// All tracing output goes to the FILE out
extern void machine_trace_execute_instr(FILE *out, bin_instr_t bi);

// Execute the given instruction, in the machine's current state
extern void machine_execute_instr(bin_instr_t bi);

// Requires: out != NULL and out can be written on
// print the state of the machine (registers, globals, and
// the memory between GPR[$sp] and GPR[$fp], inclusive) to out
extern void machine_print_state(FILE *out);

// Print instr, execute instr, then print out the machine's state (to out)
extern void machine_trace_execute(FILE *out, bin_instr_t instr);

// Invariant test for the VM (for debugging purposes)
// This exits with an assertion error if the invariant does not pass
extern void machine_okay();

#endif
