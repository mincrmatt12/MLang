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

	template<typename Iterator>
	void optimize_iterator(Iterator begin, Iterator end) {
		// while any expression in any subtree between begin and end makes optimizations.
		while (std::any_of(begin, end, [&](expression &e){return for_all_expr(e, true, [&](auto &ee){
			return optimize_flatten(ee) || optimize_deadcode(ee) || 
			       optimize_constfold(ee);
		});})) {};
	}

	// optimization techniques
	
	// returns int = number of changes made
	int optimize_flatten(expression &e);
	int optimize_deadcode(expression &e);
	int optimize_constfold(expression &e);
};

#endif
