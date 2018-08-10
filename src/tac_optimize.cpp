#include "tac_optimize.h"
#include <cmath>
#include "flow.h"

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
			optimize_deadcode() ||
			optimize_jumpthread() ||
			optimize_deduplicate() ||
			optimize_copyelision()
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

	auto info = access_info(*optimizing, false, false);

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
		if (!s->has_side_effects()) {
			// Check if s writes to something which is never read from
			std::size_t writes_with_no_reason = 0;
			std::size_t index = 0;
			s->for_all_write([&](addr_ref &f){
					if (ai_reg(f)){
						if (info.data[s].parameters[index++].empty()) {
							// No-one recalls reading this value
							++writes_with_no_reason;
						}	
					}
			});
			if (writes_with_no_reason) {
				// We have reached peak stupidity, replace this with a bloody nop for christ's sake.
				s->make_nop();
				++modifications;
				std::cout << "removed dead store" << std::endl;
			}
		}
	});

	return modifications;
}

int  tacoptimizecontext::optimize_deduplicate() {
	int modifications = 0;

	std::list<statement *> used;
	// Iterate over the entire tree, searching for identical instructions and adding them to a multimap
	// YES I KNOW THIS IS CRAPPY
	for (auto &s : traverse_v(optimizing->start)) {
		// check if s is equal to something in used
		if (auto i = std::find_if(used.begin(), used.end(), [&](auto &a){return *a == *s;}); i != used.end()) {
			// the statement is already used in exactly the same form. convert s into a nop and set its next to the deduplicated
			s->make_nop();
			s->next = *i;
			++modifications;
			std::cout << "deduplicated code" << std::endl;
		}
		else {
			used.push_back(s);
		}
	}

	return modifications;
}

int  tacoptimizecontext::optimize_copyelision() {
	int modifications = 0;

	// First, try and remove useless casts.
	// A useless cast is defined like this:
	//
	// Any cast statement who's only sources are mov statements with literals
	// and where all of those mov sources are only ever read by the cast.
	//
	// e.g:
	//
	// mov r5<16> #0<16>
	// cast r7<64> r5<16>
	// 
	// We also must do nessecary operations on the literal value to simulate
	// the cast from the old type to the new type if it is smaller.
	
	auto info = access_info(*optimizing, false, false);

	traverse_f(optimizing->start, [&](statement *s){
			// Check if this is a cast.
			if (si_cast(*s) && ai_reg(s->rhs())) {
				// Check the access info: Where does the parameter come from?
				if (std::all_of(info.data[s].parameters[1].begin(), info.data[s].parameters[1].end(), [&](auto &source){
					auto b_ = std::get_if<statement *>(&source);
					if (b_ == nullptr) return false;
					auto b = *b_;
					auto &d = info.data[b].parameters[0];
					return si_mov(*b) && ai_num(b->rhs()) && d.size() == 1 && std::all_of(d.begin(), d.end(), [&](auto &source2){
							return std::get_if<statement *>(&source2) != nullptr && *std::get_if<statement *>(&source2) == s;
					});
				})) {
					// Ok! All sources of this cast are movs and all uses of the movs are this cast only. This means we can convert the literal types
					// inside of the movs to the right types and change the registers to this one's store.
					
					// Grab the new target register
					addr_ref tgt = s->lhs();
					ex_rtype lit_type = s->lhs().rt;
					++modifications;
					int new_size = lit_type.ptr == nullptr ? lit_type.size : 64;

					for (auto &source : info.data[s].parameters[1]) {
						auto mov_stmt = *std::get_if<statement *>(&source);
						ex_rtype old_type = mov_stmt->rhs().rt;

						// Simulate the downcasting if applicable
						mov_stmt->rhs().num &= (1<<new_size)-1;
						
						// Change this mov statements rhs type to the lit_type
						mov_stmt->rhs().rt = ex_rtype(lit_type);
						// Change the target of the mov to tgt
						mov_stmt->lhs() = addr_ref(tgt);
						
						std::cout << "simplified a useless cast" << std::endl;
					}

					s->make_nop();
				}
			}
	});


	return modifications;
}

int  tacoptimizecontext::optimize_jumpthread() {
	int modifications = 0;

	traverse_f(optimizing->start, [&](statement *s){
			// Check if the pointed at statement is an ifnz, and the next statement is an ifnz, and the parameters are the same
			while (si_ifnz(*s) && s->next != nullptr && si_ifnz(*s->next) && s->lhs() == s->next->lhs() && !s->lhs().is_volatile()
					&& s->next != s->next->next) {
				// next pointer is only executed if condition is false, the condition is always
				// false since lhs is not volatile. this means that the cond pointer is never ran, so the entire statement can be skipped
				// from the first statement
				std::cout << "ifnz threaded";
				++modifications;
				s->next = s->next->next;
			}
			while (si_ifnz(*s) && s->cond != nullptr && si_ifnz(*s->cond) && s->lhs() == s->cond->lhs() && !s->lhs().is_volatile()
					&& s->cond != s->cond->cond) {
				// next pointer is only executed if condition is false, the condition is always
				// false since lhs is not volatile. this means that the cond pointer is never ran, so the entire statement can be skipped
				// from the first statement
				std::cout << "ifnz threaded";
				++modifications;
				s->cond = s->cond->cond;
			}

			// Check if the ifnz has a literal condition
			while (si_ifnz(*s) && ai_num(s->lhs())) {
				// hardcode the jump
				s->next = s->lhs().num ? s->cond : s->next;
				s->make_nop();
				std::cout << "literal ifnz hardcoded (type 1)" << std::endl;
				++modifications;
			}

			while (si_ifnz(*s) && s->next == s->cond) {
				s->make_nop();
				std::cout << "redundant ifnz removed" << std::endl;
				++modifications;
			}

			while ((si_mov(*s) || si_cast(*s)) && s->next != nullptr && si_ifnz(*s->next) && ai_num(s->rhs()) && s->lhs() == s->next->lhs()) {
				// hardcode our next with the result of of the ifnz
				s->next = s->rhs().num ? s->next->cond : s->next->next;
				std::cout << "literal ifnz hardcoded (type 2)" << std::endl;
				++modifications;
			}

	});

	return modifications;
}
