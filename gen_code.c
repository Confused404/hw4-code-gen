#include "gen_code.h"

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();
}

static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs)) {
	bin_instr_t inst = code_seq_first(cs)->instr;
	instruction_write_bin_instr(bf, inst);
	cs = code_seq_rest(cs);
    }
}

// Return a header appropriate for the give code
static BOFHeader gen_code_program_header(code_seq main_cs)
{
    BOFHeader ret;
    ret.text_start_address = 0;
    // remember, the unit of length in the BOF format is a byte!
    ret.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;
    int dsa = ret.text_length + BYTES_PER_WORD;
    ret.data_start_address = dsa;
    ret.data_length = literal_table_size() * BYTES_PER_WORD;
    int sba = dsa
	+ ret.data_start_address // double?
    + ret.data_length 
    + 4096;
    ret.stack_bottom_addr = sba;
    return ret;
}

static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
	word_type w = literal_table_iteration_next();
	// debug_print("Writing literal %f to BOF file\n", w);
	bof_write_word(bf, w);
    }
    literal_table_end_iteration(); // not necessary
}

// Requires: bf is open for writing in binary
// Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}


// Requires: bf if open for writing in binary
// Generate code for prog into bf
void gen_code_program(BOFFILE bf, block_t prog)
{
    code_seq cs = gen_code_block(prog); 
    cs = code_seq_concat(cs, code_exit());
    gen_code_output_program(bf, cs);
}

// Requires: bf if open for writing in binary
// Generate code for the given AST
code_seq gen_code_block(block_t blk)
{
    // I think that this is where all the different types of code are walked through
    code_seq ret = gen_code_const_decls(blk.const_decls);
    ret = code_seq_concat(ret, gen_code_var_decls(blk.var_decls));
    ret = code_seq_concat(ret, gen_code_proc_decls(blk.proc_decls));
    ret = code_seq_concat(ret, gen_code_stmt(blk.stmt)); 
    return ret;
}

// Generate code for the const-decls, cds
// There are 3 instructions generated for each identifier declared
// (one to allocate space and two to initialize that space)
code_seq gen_code_const_decls(const_decls_t cds)
{
    code_seq ret = code_seq_empty();
    const_decl_t *cdp = cds.const_decls;
    while(cdp != NULL)
    {
        // generate these in reverse order,
	    // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_const_decl(*cdp), ret);
        cdp = cdp->next;
    }
    return ret;
}

// Generate code for the const-decl, cd
code_seq gen_code_const_decl(const_decl_t cd)
{
    return gen_code_const_defs(cd.const_defs);
}

// Generate code for the const-defs, cdfs
code_seq gen_code_const_defs(const_defs_t cdfs)
{
    code_seq ret = code_seq_empty();
    const_def_t *cdfp = cdfs.const_defs;
    while(cdfp != NULL)
    {
        // generate these in reverse order,
        // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_const_def(*cdfp), ret);
        cdfp = cdfp->next;
    }
    return ret;
}

// Generate code for the const-def, cdf
code_seq gen_code_const_def(const_def_t cdf)
{
    int offset = id_use_get_attrs(cdf.ident.idu)->offset_count; 
    // do I need something else here or do i only need to generate code for ident and number?
    // allocate space on stack, generate code for number, pop stack into reg, sw(Sp, t9, ofst)
    code_seq ret = code_allocate_stack_space(BYTES_PER_WORD);
    ret = code_seq_add_to_end(ret, gen_code_number(cdf.number)); // (ident = number)
    ret = code_seq_add_to_end(ret, code_pop_stack_into_reg(T9));
    ret = code_seq_add_to_end(ret, code_sw(SP, T9, offset));
    return ret;
}

// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while(vdp != NULL)
    {
        // generate these in reverse order,
        // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_var_decl(*vdp), ret);
        vdp = vdp->next;
    }
    return ret;
}

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_idents(vd.idents);
}

// Generate code for the identififers in idents
// in reverse order (so the first declared are allocated last).
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_idents(idents_t idents)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = idents.idents;
    while(idp != NULL)
    {
        // generate these in reverse order,
        // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_ident(*idp), ret);
        idp = idp->next;
    }
    return ret;
}

// (Stub for:) Generate code for the procedure declarations
code_seq gen_code_proc_decls(proc_decls_t pds)
{
    code_seq ret = code_seq_empty();
    proc_decl_t *pdp = pds.proc_decls;
    while(pdp != NULL)
    {
        // generate these in reverse order,
        // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_proc_decl(*pdp), ret);
        pdp = pdp->next;
    }
    return ret;
}

// (Stub for:) Generate code for a procedure declaration
code_seq gen_code_proc_decl(proc_decl_t pd)
{
    //proc_decl_t contains a block and a name
    //code_seq ret = gen_code_block(*(pd.block));
    //no need
    bail_with_error("not supposed to be inside proc_decl");
    return NULL;
}

// Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind) 
    {
        case assign_stmt:
    	    return gen_code_assign_stmt(stmt.data.assign_stmt);
    	    break;
        case call_stmt:
            return gen_code_call_stmt(stmt.data.call_stmt);
    	    break;        
        case begin_stmt:
    	    return gen_code_begin_stmt(stmt.data.begin_stmt);
    	    break;
        case if_stmt:
    	    return gen_code_if_stmt(stmt.data.if_stmt);
    	    break;
        case while_stmt:
        	return gen_code_while_stmt(stmt.data.while_stmt);
            break;
        case read_stmt:
    	    return gen_code_read_stmt(stmt.data.read_stmt);
    	    break;
        case write_stmt:
    	    return gen_code_write_stmt(stmt.data.write_stmt);
    	    break;
        case skip_stmt:
    	    return gen_code_skip_stmt(stmt.data.skip_stmt);
    	    break;
        default:
    	    bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
    	    break;
    }
        // The following can never execute, but this quiets gcc's warning
    return code_seq_empty();
}

// Generate code for stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{
    code_seq ret = gen_code_expr(*(stmt.expr));

    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0));

    ret = code_seq_concat(ret, code_compute_fp(T9, stmt.idu->levelsOutward));

    id_attrs * attrs = id_use_get_attrs(stmt.idu);
    unsigned int offset_count = attrs->offset_count;

    ret = code_seq_add_to_end(ret, code_sw(T9, V0, offset_count));

    return ret;

}

// Generate code for stmt
code_seq gen_code_call_stmt(call_stmt_t stmt)
{
    //no need
    bail_with_error("not supposed to be inside call_stmt");
    return NULL;
}

// Generate code for stmt
code_seq gen_code_begin_stmt(begin_stmt_t stmt)
{
    code_seq ret;
    ret = gen_code_stmts(stmt.stmts);
    return ret;
}

// Generate code for the list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts)
{
    code_seq ret = code_seq_empty();
    stmt_t *stp = stmts.stmts;
    while(stp != NULL)
    {
        // generate these in reverse order,
        // so the addressing offsets work properly
        ret = code_seq_concat(gen_code_stmt(*stp), ret);
        stp = stp->next;
    }
    return ret;
}

// Generate code for the if-statment given by stmt
code_seq gen_code_if_stmt(if_stmt_t stmt)
{
    code_seq then_stmt = gen_code_stmt(*(stmt.then_stmt));
    code_seq else_stmt = gen_code_stmt(*(stmt.else_stmt));

    int branch_size = code_seq_size(code_seq_singleton(code_beq(0, 0, 0)));
    int then_stmt_size = code_seq_size(then_stmt);
    int else_stmt_size = code_seq_size(else_stmt);

    then_stmt = code_seq_concat(then_stmt, code_beq(0, 0, else_stmt_size + branch_size)); //concat a jump of size elsestmt
    
    code_seq ret = gen_code_condition(stmt.condition);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(T9)); //pop from stack into t9
    code_seq skip_then_stmt = code_seq_singleton(code_beq(T9, 0, then_stmt_size + branch_size));

    ret = code_seq_concat(ret, skip_then_stmt);
    ret = code_seq_concat(ret, then_stmt);
    ret = code_seq_concat(ret, else_stmt);

    return ret;
}

code_seq gen_code_while_stmt(while_stmt_t stmt)
{
    code_seq ret;
    code_seq condition = gen_code_condition(stmt.condition);
    int condition_size = code_seq_size(condition);

    //pop from stack into t9
    ret = condition;
    ret = code_seq_concat(ret, code_pop_stack_into_reg(T9));
    //find size of body
    code_seq body = gen_code_stmt(*(stmt.body));
    int body_size = code_seq_size(body);
    //test t9 to see if it is false
    //if false skip around body
    //if true, go to body
    ret = code_seq_concat(ret, code_beq(T9, 0, body_size + 1)); 
    ret = code_seq_concat(ret, body);
    //go to next stmt 
    //jump back to condition
    ret = code_seq_concat(ret, code_beq(0, 0, -(body_size + condition_size + 1)));
    return ret;

}

// Generate code for the read statment given by stmt
code_seq gen_code_read_stmt(read_stmt_t stmt)
{
    int ofst = id_use_get_attrs(stmt.idu)->offset_count;
    code_seq ret = code_rch();
    ret = code_seq_concat(ret, code_compute_fp(T9, stmt.idu->levelsOutward));
    ret = code_seq_concat(ret, code_sw(T9, V0, ofst));
    return ret;
}

// Generate code for the write statment given by stmt.
code_seq gen_code_write_stmt(write_stmt_t stmt)
{
    code_seq ret = gen_code_expr(stmt.expr);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(A0));
    return code_seq_add_to_end(ret, code_pint());
}

// Generate code for the skip statment, stmt
code_seq gen_code_skip_stmt(skip_stmt_t stmt)
{
    code_seq ret = code_srl(AT, AT, 0);
    return ret;
}

// Requires: reg != T9
// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// May modify HI,LO when executed
code_seq gen_code_condition(condition_t cond)
{
    code_seq ret;
    switch(cond.cond_kind)
    {
        case ck_odd:
            ret = gen_code_odd_condition(cond.data.odd_cond);
            break;
        case ck_rel:
            ret = gen_code_rel_op_condition(cond.data.rel_op_cond);
            break;
        default:
            bail_with_error("Call to gen_code_condition with an AST that is not a condition!");
            break;
    }
    return ret;
}

// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// Modifies SP, HI,LO when executed
code_seq gen_code_odd_condition(odd_condition_t cond)
{
    code_seq ret = gen_code_expr(cond.expr);
    return ret;
}

// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_rel_op_condition(rel_op_condition_t cond)
{
    code_seq ret = gen_code_expr(cond.expr1);
    ret = code_seq_concat(ret, gen_code_expr(cond.expr2));
    ret = code_seq_concat(ret, gen_code_rel_op(cond.rel_op));
    return ret;
}

// Generate code for the rel_op
// applied to 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_rel_op(token_t rel_op) // WORKED ON THIS
{
    code_seq ret = code_pop_stack_into_reg(AT);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0));
    switch(rel_op.code)
    {
        case eqsym:
            ret = code_seq_concat(ret, code_beq(V0, AT, 2));
            break;
        case neqsym:
            ret = code_seq_concat(ret, code_bne(V0, AT, 2));
            break;
        case ltsym: 
            ret = code_seq_concat(ret, code_sub(V0, AT, V0));
            ret = code_seq_concat(ret, code_bltz(V0, 2));
            break;
        case gtsym: 
            ret = code_seq_concat(ret, code_sub(V0, AT, V0));
            ret = code_seq_concat(ret, code_bgtz(V0, 2));
            break;
        case leqsym:
            ret = code_seq_concat(ret, code_sub(V0, AT, V0));
            ret = code_seq_concat(ret, code_blez(V0, 2));
            break; 
        case geqsym:
            ret = code_seq_concat(ret, code_sub(V0, AT, V0));
            ret = code_seq_concat(ret, code_bgez(V0, 2));
            break;
        default:
            bail_with_error("Call to gen_code_rel_op with an AST that is not a relop!");
            break;
    }
    ret = code_seq_concat(ret, code_add(0, 0, V0));
    ret = code_seq_concat(ret, code_beq(0, 0, 1)); 
    ret = code_seq_concat(ret, code_addi(0, V0, 1));
    ret = code_seq_concat(ret, code_push_reg_on_stack(V0));
    return ret;
}

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_expr(expr_t exp)
{
    code_seq ret;
    switch(exp.expr_kind)
    {
        case expr_bin:
            ret = gen_code_binary_op_expr(exp.data.binary);
            break;
        case expr_ident:
            ret = gen_code_ident(exp.data.ident);
            break;
        case expr_number:
            ret = gen_code_number(exp.data.number);
            break;
        default:
            bail_with_error("Call to gen_code_expr with an AST that is not an expression!");
            break;
    }
    return ret;
}

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    code_seq ret = gen_code_expr(*(exp.expr1));
    ret = code_seq_concat(ret, gen_code_expr(*(exp.expr2)));
    ret = code_seq_concat(ret, gen_code_arith_op(exp.arith_op));
    return ret;
}

// Generate code to apply arith_op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_arith_op(token_t arith_op) // reference relop for this
{
    code_seq ret;
    ret = code_pop_stack_into_reg(T2);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(T1));

    switch(arith_op.code)
    {
        case plussym:
            ret = code_seq_concat(ret, code_add(T1, T2, T1)); 
            break;
        case minussym:
            ret = code_seq_concat(ret, code_sub(T1, T2, T1));
            break;
        case multsym:
            ret = code_seq_concat(ret, code_mul(T1, T2)); 
            ret = code_seq_concat(ret, code_mflo(T1));
            break;
        case divsym:
            ret = code_seq_concat(ret, code_div(T1, T2));
            ret = code_seq_concat(ret, code_mflo(T1));
            break;
        default:
            bail_with_error("Call to gen_code_binary_op_expr with an AST that is not an expression!");
            break;
    }
    ret = code_seq_concat(ret, code_push_reg_on_stack(T1));
    return ret;
}

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
code_seq gen_code_ident(ident_t id)
{
    //get offset of variable, lw(fp, reg, ofst), push reg on stack
    int ofst = id_use_get_attrs(id.idu)->offset_count;
    code_seq ret = code_lw(FP, T9, ofst);
    ret = code_seq_concat(ret, code_push_reg_on_stack(T9));
    return ret;
}

// Generate code to put the given number on top of the stack
code_seq gen_code_number(number_t num)
{
    // literal table lookup
    unsigned int offset = literal_table_lookup(num.text, num.value);
    code_seq ret = code_lw(GP, T9, offset);
    ret = code_seq_concat(ret, code_push_reg_on_stack(T9));
    return ret;
    // i forgot what else he said to do but it cant be more than 1 or 2 extra lines ;(
    
}
