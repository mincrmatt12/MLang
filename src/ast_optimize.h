// ast_optimize.h - interface to the ast optimizer
//
// creates an astoptimizecontext object for storing the optimize context.
//
// runs in a few phases:
// 	first: determine the side effects of functions
//     second: make optimizations in a loop
//      third: ... there were only two phases :)

#ifndef AST_OPTIMIZE_H
#define AST_OPTIMIZE_H

#include <parser.h>
#include <map>
#include "expr_utils.h"

struct astoptimizecontext {
	// these are moved out of the parsecontext
	std::vector<function> functions; 
	std::map<std::string, expression> global_inits;

	// moved out so that these can be moved when compiling
	std::vector<ext_function> ext_functions;

	// keeping track of function purity
	std::map<std::string, bool> purity;

	astoptimizecontext(parsecontext &&ctx) {
		functions = std::move(ctx.func_list);
		global_inits = std::move(ctx.global_initializers);
		ext_functions = std::move(ctx.ext_list);
	}

	bool is_pure(const expression &e); // is this expression pure?
	void find_pure_funcs();

	void optimize();
};

#endif
