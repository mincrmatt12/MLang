#include "ast_optimize.h"

void astoptimizecontext::find_pure_funcs() {
	purity.clear(); // reset pure functions

	// value is true if it has side effects
	
	// a function has side effects if:
	// 	it calls an extfunc
	// 	it assigns to any of
	// 		static var
	// 		dereference
	// 	it calls a known side effects function
	
	// checking this requires a loop
	do {} while (std::count_if(this->functions.begin(), this->functions.end(), [&](function &f){
		// count these returns, if anything is marked it returns true causing us to loop again
		
		if (purity.count(f.name) != 0) return false;
		std::cout << "pure checking " << f.name << std::endl;

		bool unchecked_encountered = false;

		// iterate over all expressions, searching for something matching the above descriptions
		bool side_effects = for_all_expr(f.code, true, [&](const expression &ee){
			if (is_assign(ee)) { return for_all_expr(ee.params.back(), true, is_deref) || for_all_expr(ee.params.back(), true, [&](const expression &eee){
				return (is_ident(eee) && eee.ident.type == id_type::global_var); }); } // checks assignment operators
			if (is_fcall(ee)) {
				const auto &tgt = ee.params.front();
				
				if (!tgt.is_compiletime_expr()) return true; // also catches extfuncs as they are not considered compile time exprs

				const auto &called = this->functions[tgt.ident.index];

				if (purity.count(called.name) != 0 && !purity.at(called.name)) return true; // the function is known to be impure
				if (purity.count(called.name) == 0 && called.name != f.name) {
					// unknown function, wait for next round
					unchecked_encountered = true;
					std::cout << "next iter for " << called.name << std::endl;
				}
			}
			return false;
		});

		if (side_effects || !unchecked_encountered) { // is this function definitely something
			purity.emplace(f.name, !side_effects);
			return true;
		}
		return false;
	}));
	for (auto &f : functions) {
		if (purity.count(f.name) == 0)
			std::cerr << "forgot to check " << f.name << std::endl;
	}
}

void astoptimizecontext::optimize() {
	// todo
	this->find_pure_funcs();
}
