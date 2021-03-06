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
	std::vector<expression> global_inits;

	// moved out so that these can be moved when compiling
	std::vector<ext_function> ext_functions;

	// keeping track of function purity
	std::map<std::string, bool> purity;

	astoptimizecontext(parsecontext &&ctx) {
		functions = std::move(ctx.func_list);
		global_inits = std::move(ctx.global_initializers);
		ext_functions = std::move(ctx.ext_list);
	}

	bool is_pure(const expression &e, bool typeonly=false); // is this expression pure?
	void find_pure_funcs();

	void optimize();

	template<typename Iterator>
	void optimize_iterator(Iterator begin, Iterator end) {
		// while any expression in any subtree between begin and end makes optimizations.
		while (std::any_of(begin, end, [&](expression &e){return for_all_expr(e, true, [&](auto &ee){
			return optimize_flatten(ee)         || optimize_deadcode(ee)          || 
			       optimize_arith_constfold(ee) || optimize_pointer_simplify(ee)  || optimize_simplify_casts(ee);
		});})) {find_pure_funcs();};
		// some techniques depend on other ones to have finished. run them now
		while (std::any_of(begin, end, [&](expression &e){return for_all_expr(e, true, [&](auto &ee){
			return optimize_flatten(ee)         || optimize_deadcode(ee)         ||
			       optimize_arith_constfold(ee) || optimize_pointer_simplify(ee) ||
			       optimize_logicalfold(ee)     || optimize_simplify(ee)         ||
			       optimize_simplify_casts(ee);
		});})) {find_pure_funcs();};
	}

	// optimization techniques

	// returns int = number of changes made
	int optimize_flatten                    (expression &e);
	int optimize_deadcode                   (expression &e);
	int optimize_arith_constfold            (expression &e);
	int optimize_pointer_simplify           (expression &e);
	int optimize_logicalfold                (expression &e);
	int optimize_simplify                   (expression &e);
	int optimize_simplify_casts		(expression &e);
};

#endif
