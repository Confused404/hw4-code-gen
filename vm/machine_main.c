/* $Id: machine_main.c,v 1.5 2023/11/14 18:25:32 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bof.h"
#include "machine.h"
#include "utilities.h"

/* Print a usage message on stderr and exit with exit code 1. */
static void usage(const char *cmdname)
{
    fprintf(stderr, "Usage: %s file.bof\n", cmdname);
    fprintf(stderr, "   or: %s -p file.bof\n", cmdname);
    bail_with_error("   or: %s -t file.bof", cmdname);
}

// Run the VM on the .bof file name given in argv[1]
int main(int argc, char *argv[])
{
    const char *cmdname = argv[0];
    argc--;
    argv++;

    bool print_program = false;
    bool should_trace = false;
    if (argc == 2 && strcmp(argv[0], "-p") == 0) {
	print_program = true;
	argc--;
	argv++;
    }

    if (argc == 2 && strcmp(argv[0], "-t") == 0) {
	should_trace = true;
	argc--;
	argv++;
    }

    if (print_program && should_trace) {
	bail_with_error("Cannot both print the program (with -p) and trace it (with -t)!");
    }

    // now there should be exactly 1 file argument
    if (argc != 1 || argv[0][0] == '-') {
	usage(cmdname);
    }

    char *suffix = strchr(argv[0], '.');
    if (suffix == NULL || strcmp(suffix, ".bof") != 0) {
	usage(cmdname);
    }

    BOFFILE bf = bof_read_open(argv[0]);

    machine_load(bf);

    // if printing, don't run the program
    if (print_program) {
	machine_print_loaded_program(stdout);
	return EXIT_SUCCESS;
    }
    
    machine_run(should_trace);

    // the following should never execute,
    // as the exit system call will call exit(0);
    return EXIT_SUCCESS;
}
