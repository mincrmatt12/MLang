#include "tac_optimize.h"

tacoptimizecontext::tacoptimizecontext(compiler &&c) {
	all_statements = std::move(c.all_statements);
	global_initscope = std::move(c.global_initscope);
	string_table = std::move(c.string_table);
	ext_list = std::move(c.ext_list);
	func_compileunits = std::move(c.func_compileunits);
}

void tacoptimizecontext::optimize_unit(compilation_unit &u) {
	optimizing = &u; // set the current optimizing blocky thingy madoodle

	std::cout << "optimizing" << std::endl;
	while (
			optimize_deadcode()
	) {}
}

void tacoptimizecontext::optimize() {
	for (auto &[_, cu] : func_compileunits) {
		optimize_unit(cu);
	}
	optimize_unit(global_initscope);
	std::cout << "optimized" << std::endl;
}

int  tacoptimizecontext::optimize_deadcode() {
	int modifications = 0;

	// Generate a tree traversal.
	// Go through ~to ALL THE WORLD~ and find nops and remove them
	
	traverse_f(optimizing->start, [&](statement *s){
		// Check if the next pointer or cond pointer points to a nop
		// and change it to the nops next pointer
		if (s->next != nullptr && si_nop(*s->next)) {
			s->next = s->next->next;
			++modifications;
			std::cout << "removed a nop" << std::endl;
		}
		if (s->cond != nullptr && si_nop(*s->cond)) {
			s->cond = s->cond->next;
			++modifications;
			std::cout << "removed a nop" << std::endl;
		}
		// Make sure rets don't go anywhere
		if (si_ret(*s) && s->next != nullptr) {
			s->next = nullptr;
			++modifications;
			std::cout << "stubbed a ret" << std::endl;
		}
	});

	return modifications;
}
