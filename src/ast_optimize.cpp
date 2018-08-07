#include <numeric>
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
	this->optimize_iterator(this->global_inits.begin(), this->global_inits.end());
}	

bool astoptimizecontext::is_pure(const expression &e, bool typeonly) {
	if (is_fcall(e) && !typeonly) {
		if (!e.is_compiletime_expr()) return false;
		if (purity.count(e.ident.name) == 0) return false;
		return purity[e.ident.name];
	}
	return !e.has_side_effects(typeonly);
}

int astoptimizecontext::optimize_flatten(expression &e) {
	int modifications = 0;
	if (is_add(e) || is_mul(e) || is_l_or(e) || is_l_and(e) || is_comma(e) || is_div(e)) {
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
		case 1: if(is_add(e) || is_mul(e) || is_comma(e) || is_div(e)) {e = expression(std::move(e.params.front()));         ++modifications;}
			else if (is_l_or(e) || is_l_and(e))                    {e = e_eq(e_eq(std::move(e.params.front()), 0l), 0l); ++modifications;}
			break;
		case 0: if (is_add(e) || is_mul(e) || is_l_or(e) || is_div(e)) {e = 0l;                                              ++modifications;}
			else if (is_l_and(e))                                  {e = 1l;                                              ++modifications;}
	}
	return modifications;
}

int astoptimizecontext::optimize_deadcode(expression &e) {
	int modifications = 0;
	if (!(is_loop(e) || is_comma(e))) return 0;
	if (is_loop(e) && is_literal_number(e.params.front()) && e.params.front().numvalue == 0) {e = e_nop(); std::cout << "erased pointless loop" << std::endl; return 1;}

	// Remove any expression with no side effects except for the last one
	// Additionally, take things that aren't pure but that don't contribute to anything and place the non-pure operands
	// directly into the comma operator:
	//
	// |- comma
	// | |- add
	// | | |- literal_number 2
	// | | |  fcall 
	// | | |  |- ident	(non_pure)
	//
	// becomes
	//
	// |- comma
	// | |- fcall
	// | | |- ident	(non_pure)
	for (auto i = e.params.begin(); i != e.params.end(); ) {
		if (is_loop(e)) {
			if (i == e.params.begin()) {++i; continue;} // Skip condition in loop processing
		}	
		else {
			if (std::next(i) == e.params.end()) {break;} // Skip returned expression in comma
		}

		// First, check if the expression has no side effects. If so, erase it from the list
		if (is_pure(*i)) {
			// In this case, we can just ELIMINATE IT
			// FATALITY
			std::cout << "removed pure expression from comma or loop" << std::endl;
			i = e.params.erase(i);
		}
		else {
			// Otherwise, check if the expression type on its own has side effects
			if (!is_pure(*i, true)) {
				++i;  // The expression can stay	
			}
			else {
				// Otherwise, we may be able to just move the arguments out as long as the expression does not short circuit. Check that now:
				switch (i->t) {
					default:
						++i;
						break;
					case ex_type::add:
					case ex_type::neg:
					case ex_type::eq:
					case ex_type::mul:
					case ex_type::div:
					case ex_type::addr:
					case ex_type::deref:
					case ex_type::comma:
						auto tmp(std::move(i->params));
						std::cout << "adopted parameters from sub-expr" << std::endl;
						e.params.splice(i = e.params.erase(i), std::move(tmp));
				}
			}
		}
	}

	// Remove anything after a return statement or infinite loop, if any.
	if (auto loc = std::find_if(e.params.begin(), e.params.end(), [&](expression &p){ return is_ret(p) || (is_loop(p) && is_literal_number(p.params.front()) && p.params.front().numvalue == 1);}); loc != e.params.end()) {
		size_t length = std::distance(e.params.begin(), loc) + 1;
		if (length != e.params.size()) {
			e.params.resize(length);
			std::cout << "removed inaccessible code" << std::endl;
			++modifications;
		}
	}

	return modifications;
}

int astoptimizecontext::optimize_arith_constfold(expression &e) {
	int modifications = 0;
	// Constant folding for neg:
	// |- neg
	// | |- literal_number 2
	//
	// becomes
	//
	// |- literal_number -2
	
	if (is_neg(e)) {
		if (is_literal_number(e.params.front())) {
			++modifications;
			std::cout << "folded neg" << std::endl;
			e = -e.params.front().numvalue;
		}
		else if (is_neg(e.params.front())) {
			++modifications;
			std::cout << "removed extraneous neg" << std::endl;
			e = expression(std::move(e.params.front().params.front()));
		}
	}

	// Constant folding on mathematic types 
	//
	// |- add
	// | |- literal_number 1
	// | |- literal_number 2
	//
	// becomes
	//
	// |- add
	// | |- literal_number 3
	//
	// which is then optimized further by the tree flatenner
	if (is_add(e) || is_mul(e) || (
		is_div(e) && std::all_of(e.params.begin(), e.params.end(), is_literal_number)				
	)) {
		// Make sure there are at least two numerical arguments to avoid pessimization
		if (std::count_if(e.params.begin(), e.params.end(), is_literal_number) >= 2) {
			// Extract all of the constants into another type
			std::vector<long> constants{};
			e.params.remove_if([&](expression &h){
				if (is_literal_number(h)) constants.emplace_back(h.numvalue); // copy out of the list
				return is_literal_number(h);
			});
			// Aggregate the values together using std::accumulate
			long aggregate;
			switch (e.t) {
				case ex_type::add: aggregate = std::accumulate(constants.begin(), constants.end(), 0)                          ; break ;
				case ex_type::mul: aggregate = std::accumulate(constants.begin(), constants.end(), 1, std::multiplies<long>()) ; break ;
				case ex_type::div: aggregate = std::accumulate(++constants.begin(), constants.end(), *(constants.begin()), std::divides<long>()) ; break ;
				default: break                                                                                                 ;
			}

			// Add the number constant back into the expression
			e.params.emplace_back(aggregate);

			// Count the modification
			++modifications;
			std::cout << "folded " << constants.size() << " constants into one." << std::endl;
		}
	}
	// Remove 0's from adds
	if (is_add(e)) {
		e.params.remove_if([&](expression &h){
				if (is_literal_number(h) && h.numvalue == 0) {
					std::cout << "removed 0 from add" << std::endl;
					++modifications;
					return true;
				}
				return false;
		});
	}
	// Remove 1's from multiplies and divides
	// UNLESS the 1 is in the first place and the type is div
	if (is_mul(e) || is_div(e)) {
		int i = 0;
		e.params.remove_if([&](expression &h){
				++i;
				if (is_literal_number(h) && h.numvalue == 1 && (e.t == ex_type::div && i != 1)) {
					std::cout << "removed 1 from mul/div" << std::endl;
					++modifications;
					return true;
				}
				return false;
		});
	}
	// Change multiply and divide with zeros in them
	// e.g
	//
	// |- mul
	// | |- ...
	// | |- literal_number 0
	//
	// to 
	//
	// |- literal_number 0
	//
	// For divide, only check if the first parameter is a zero.
	//
	if ((is_mul(e) && std::any_of(e.params.begin(), e.params.end(), [&](expression &d){return is_literal_number(d) && d.numvalue == 0;})) || 
	    (is_div(e) && e.params.size() >= 1 && is_literal_number(e.params.front()) && e.params.front().numvalue == 0)) {
		++modifications;
		e = 0l;
	}

	// TODO:
	//
	// |- div
	// | |- literal_number 1
	// | |- literal_number 1
	// | |- ...
	//
	// to
	//
	// |- div
	// | |- literal_number 1
	// | |- ...
	//
	// for any number
	
	// If there are more neg parameters in an add then non-neg invert everything
	if (is_add(e) && std::count_if(e.params.begin(), e.params.end(), is_neg) > long(e.params.size()/2)) {
		for (auto &p : e.params) p = e_neg(std::move(p));
		e = e_neg(std::move(e));
	}
	return modifications;
}

int astoptimizecontext::optimize_pointer_simplify(expression &e) {
	int modifications = 0;
	// Simplify expressions of the form
	//
	// *&(s)
	// &*(s)
	//
	// to just s
	if (is_addr(e)) {
		if (is_deref(e.params.front())) {
			++modifications;
			std::cout << "simplified &*" << std::endl;
			e = expression(std::move(e.params.front().params.front()));	
		}	
	}
	if (is_deref(e)) {
		if (is_addr(e.params.front())) {
			++modifications;
			std::cout << "simplified *&" << std::endl;
			e = expression(std::move(e.params.front().params.front()));	
		}	
	}
	return modifications;
}

int astoptimizecontext::optimize_logicalfold(expression &e) {
	int modifications = 0;
	if (!(is_l_or(e) || is_l_and(e))) return 0;

	auto magic_val = is_l_or(e) ? [](long &v){return v != 0;} : [](long &v){return v == 0;};
	// Consider this expression:
	//
	// b && a && c && 0 && e
	// where b, e and c has no side effects and a does.
	// 
	// This could be simplified to
	// (b && a, 0)
	// since c can be removed without any effect, e will never be executed and b determines if a is executed so it must be kept
	//
	// Essentially, we must find the first pure value from a reverse iterator starting at the first 0 and copy the range given by it and the beginning
	// to a new expression inside of a comma expr with 0.
	if (auto t = std::find_if(e.params.begin(), e.params.end(), [&](expression &p){
		return is_literal_number(p) && magic_val(p.numvalue);	
	}); t != e.params.end()) {
		++modifications;
		std::cout << "folding logical: ";
		// OK! t is now an iterator pointing directly at the 0. We now need to find if there are any non-pure expressions before here		
		if (t == e.params.begin()) {
			good:
				std::cout << "using simple fixer" << std::endl;
				e = is_l_or(e) ? 1l : 0l;
		}
		else {
			auto res = std::find_if(std::make_reverse_iterator(std::prev(t)), std::make_reverse_iterator(e.params.begin()), [&](auto &h){return is_pure(h);});
			if (res == std::make_reverse_iterator(e.params.begin())) goto good;
			// Alright, res is now a reverse iterator that we can convert to a normal pointer.
			auto r_end = res.base();
			auto n_e = e_l_and();
			n_e.params.splice(n_e.params.begin(), e.params, e.params.begin(), std::next(r_end));
			std::cout << "using complex fixer" << std::endl;
			e = e_comma(std::move(n_e), is_l_or(e) ? 1l : 0l);
		}
	}

	// Consider this expression:
	//
	// a && a
	// or
	// a || a
	//
	// These can be simplified to just a or b.
	// The flattenner will pick these up, so simply remove any identical expressions that are pure
	if ((is_l_or(e) || is_l_and(e))) {
		// Make sure we haven't changed out the things
		for (auto i = std::next(e.params.begin()); i != e.params.end();) {
			if (is_pure(*i) && std::find(e.params.begin(), i, *i) != i) {
				std::cout << "removing duplicate in log" << std::endl;
				++modifications;
				i = e.params.erase(i);
			}	
			else {
				++i;
			}
		}
	}

	return modifications;
}

int astoptimizecontext::optimize_simplify(expression &e) {
	// Due to my design, many optimizations are not possible due to me not being able to create temporaries due to the context-less approach used 
	// when simplifying expressions. This may change at a later date but for now I will only add optimizations that can be executed without temporaries,
	// usually meaning we cannot optimize unpure values. :(
	int modifications = 0;
	switch (e.t) {
		case ex_type::eq:
			// Check if both values are equal
			++modifications;
			if (is_literal_number(e.params.front()) && is_literal_number(e.params.back()))
				e = static_cast<long>(e.params.front().numvalue == e.params.back().numvalue);
			else if (e.params.front() == e.params.back() && is_pure(e.params.front()))
				e = 1l;
			// If values are not equal and they are both pure and are compile time expressions, we can set the result to 0
			else if (!(e.params.front() == e.params.back()) && is_pure(e.params.front()) && is_pure(e.params.back()) && e.params.front().is_compiletime_expr() && e.params.back().is_compiletime_expr())
				e = 0l;
			else --modifications;
			break;
		case ex_type::assign:
			// Check if we are doing
			// a = a
			// and a is pure
			if (e.params.back() == e.params.front() && is_pure(e.params.back())) {
				e = expression(std::move(e.params.back()));
				++modifications;
			}
			break;
		case ex_type::comma:
			// Check if the last element of the comma is the same as the preceeding assign target
			// 
			// (a=3, a)
			// becomes
			// (a=3)
			if (e.params.size() >= 2) {
				auto& last = e.params.back(), &prev = *std::next(e.params.rbegin());
				if (is_assign(prev) && prev.params.back() == last) e.params.pop_back();
			}
			break;
		default:
			break;
	}
	return modifications;
}
