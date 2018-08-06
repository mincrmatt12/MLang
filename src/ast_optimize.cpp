#include "stringify.h"
#include "transform_iterator.h"

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
	this->find_pure_funcs();
	this->optimize_iterator(make_transform_iterator(this->functions.begin(), this->functions.end(), [&](function &e) -> auto& {return e.code;}), transform_iterator<expression &>());
	for (auto &e : functions) {
		this->optimize_iterator(e.global_initializers.begin(), e.global_initializers.end());
	}
	this->optimize_iterator(make_transform_iterator(this->global_inits.begin(), this->global_inits.end(), [&](auto &e) -> auto& {return e.second;}), transform_iterator<expression &>());
}	

bool astoptimizecontext::is_pure(const expression &e) {
	if (is_fcall(e)) {
		if (!e.is_compiletime_expr()) return false;
		if (purity.count(e.ident.name) == 0) return false;
		return purity[e.ident.name];
	}
	return !e.has_side_effects();
}

int astoptimizecontext::optimize_flatten(expression &e) {
	int modifications = 0;
	if (is_add(e) || is_mul(e) || is_l_or(e) || is_l_and(e) || is_comma(e)) {
		// Adopt children of the same type as e
		expr_vec new_args{};
		for (auto &d : e.params) {
			if (d.t != e.t){new_args.emplace_back(std::move(d)); continue;}
			++modifications;
			// Adopt this expression
			new_args.splice(new_args.end(), d.params);
			std::cout << "flattened one expression in tree" << std::endl;
		}
	        e.params = std::move(new_args);
	}
	if (is_add(e)) {
		expr_vec new_args{};
		for (auto &d : e.params) {
			if (is_neg(d) && is_add(d.params.front())) {
				++modifications;
				// For each add in the neg, synthesize a new neg with the parameter in it.
				for (auto &param : d.params.front().params) {
					new_args.emplace_back(std::move(e_neg(std::move(param))));
				}
				std::cout << "simplified 1 neg expr" << std::endl;
			}
			else new_args.emplace_back(std::move(d));
		}
	        e.params = std::move(new_args);
	}

	switch (e.params.size()) {
		case 1: if(is_add(e) || is_mul(e) || is_comma(e)) e = expression(std::move(e.params.front())),         ++modifications;
			else if (is_l_or(e) || is_l_and(e))       e = e_eq(e_eq(std::move(e.params.front()), 0l), 0l), ++modifications;
			break;
		case 0: if (is_add(e) || is_mul(e) || is_l_or(e)) e = 0l,                                              ++modifications;
			else if (is_l_and(e))                     e = 1l,                                              ++modifications;
	}
	return modifications;
}

int astoptimizecontext::optimize_deadcode(expression &e) {
	int modifications = 0;
	if (!is_comma(e)) {
		return 0;	
	}
	// Remove all nops from the comma operator, as they do absolutely nothing
	modifications += std::count_if(e.params.begin(), e.params.end(), is_nop);
	e.params.remove_if(is_nop);

	return modifications;
}
