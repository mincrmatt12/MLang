// tac_optimize.h
//
//
// Optimizes TAC

#ifndef TAC_OPTIMIZE_H
#define TAC_OPTIMIZE_H

#include <parser.h>
#include "compiler.h"
#include "tac.h"

struct tacoptimizecontext {
	std::map<std::string, compilation_unit> func_compileunits;
	compilation_unit global_initscope;
	std::string string_table;
	// Used so we can pass this around into the next stage
	std::vector<ext_function> ext_list;

	std::vector<std::unique_ptr<statement>> all_statements;

	tacoptimizecontext(compiler &&c);

	compilation_unit * optimizing{nullptr};
	std::string        optimizing_name;

	void optimize_unit(compilation_unit &u);
	void optimize();

	// Optimization methods, see ast_optimize for docs on what these are supposed to be
	// I do not have a garbage collector in my code, as the code generator will never see unused statements
	// and gcc will automatically destroy them anyways at the end so all you get is a slightly larger
	// memory footprint
	
	int optimize_deadcode();  //  removes nops && code after rets
	int optimize_jumpthread();
	int optimize_deduplicate();
	int optimize_copyelision();
	int optimize_simplify();
	int optimize_rename(); // renames registers to use less of them
	int optimize_canonize(); // canonize: in commutative statements order by 

	void remove_register_holes(); // not an optimizer as it is only ran once for the entire cu
};

#endif // TAC_OPTIMIZE_H
