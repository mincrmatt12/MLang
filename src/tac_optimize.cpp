#include "tac_optimize.h"
#include <cmath>
#include "flow.h"
#include "args.h"
#include "stringify.h"

#define DUMP_T if(dumplevel >= 2)

tacoptimizecontext::tacoptimizecontext(compiler &&c) {
	all_statements = std::move(c.all_statements);
	global_initscope = std::move(c.global_initscope);
	string_table = std::move(c.string_table);
	ext_list = std::move(c.ext_list);
	func_compileunits = std::move(c.func_compileunits);
}

void tacoptimizecontext::optimize_unit(compilation_unit &u) {
	optimizing = &u; // set the current optimizing blocky thingy madoodle
	if (u.start == nullptr) {
		DUMP_T std::cout << "skipping optimize as start is null" << std::endl;
		return;
	}

	DUMP_T std::cout << "optimizing" << std::endl;
	while (
			optimize_deadcode()    ||
			optimize_jumpthread()  ||
			optimize_deduplicate() ||
			optimize_copyelision() ||
			optimize_simplify()    || 
			optimize_rename()      
			/* don't canonize for maximum impact */
	) { }
	while (
			optimize_deadcode()    ||
			optimize_jumpthread()  ||
			optimize_deduplicate() ||
			optimize_copyelision() ||
			optimize_simplify()    || 
			optimize_rename()      ||
			optimize_stackoff()    ||
			optimize_canonize()
	) {}
	remove_register_holes();
}

void tacoptimizecontext::optimize() {
	for (auto &[_, cu] : func_compileunits) {
		DUMP_T std::cout << "optimizing " << _ << std::endl;
		optimizing_name = _;
		optimize_unit(cu);
	}
	optimize_unit(global_initscope);
	optimizing_name = "$GLOBAL";
	if (dumplevel != 0) std::cout << "optimized" << std::endl;
}

int  tacoptimizecontext::optimize_deadcode() {
	int modifications = 0;

	if (do_remove_nops() && si_nop(*optimizing->start) && optimizing->start->next != nullptr) {
		optimizing->start = optimizing->start->next;
		DUMP_T std::cout << "removed nop at start" << std::endl;
		++modifications;
	}

	auto info = access_info(*optimizing, false, false);

	// Generate a tree traversal.
	// Go through ~to ALL THE WORLD~ and find nops and remove them
	
	traverse_f(optimizing->start, [&](statement *s){
		// Check if the next pointer or cond pointer points to a nop
		// and change it to the nops next pointer
		if (do_remove_nops() && s->next != nullptr && si_nop(*s->next)) {
			s->next = s->next->next;
			++modifications;
			DUMP_T std::cout << "removed a nop" << std::endl;
		}
		if (do_remove_nops() && s->cond != nullptr && si_nop(*s->cond)) {
			s->cond = s->cond->next;
			++modifications;
			DUMP_T std::cout << "removed a nop" << std::endl;
		}
		// Make sure rets don't go anywhere
		if (si_ret(*s) && s->next != nullptr) {
			s->next = nullptr;
			++modifications;
			DUMP_T std::cout << "stubbed a ret" << std::endl;
		}
		if (do_remove_deadstores() && !s->has_side_effects()) {
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
				DUMP_T std::cout << "removed dead store" << std::endl;
			}
		}
	});

	return modifications;
}

int  tacoptimizecontext::optimize_deduplicate() {
	int modifications = 0;
	if (!do_deduplicate()) return 0;

	std::list<statement *> used;
	// Traverse every statement, checking if we already saw an equivalent statement.
	// Yes I know the O(n^2) is terrible
	//
	//
	// I don't care though.
	for (auto &s : traverse_v(optimizing->start)) {
		if (si_ret(*s)) continue; // don't change returns since it usually makes more sense to leave them alone
		// check if s is equal to something in used
		if (auto i = std::find_if(used.begin(), used.end(), [&](auto &a){return *a == *s;}); i != used.end()) {
			// the statement is already used in exactly the same form. convert s into a nop and set its next to the deduplicated
			s->make_nop();
			s->next = *i;
			++modifications;
			DUMP_T std::cout << "deduplicated code" << std::endl;
		}
		else {
			used.push_back(s);
		}
	}

	return modifications;
}

inline constexpr uint64_t size_of_rtype(const ex_rtype &r) {
	return r.ptr == nullptr ? r.size : 64;
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
			if (do_simplify_useless_casts() && si_cast(*s) && ai_reg(s->rhs())) {
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
					int new_size = size_of_rtype(lit_type);

					for (auto &source : info.data[s].parameters[1]) {
						auto mov_stmt = *std::get_if<statement *>(&source);
						ex_rtype old_type = mov_stmt->rhs().rt;

						// Simulate the downcasting if applicable
						// Make a copy in a 64 bit type first though
						int64_t value = mov_stmt->rhs().num;
						if (size_of_rtype(mov_stmt->rhs().rt) > new_size) {
							value &= (1 << new_size)-1;
						}
						mov_stmt->rhs().num = value;
						
						// Change this mov statements rhs type to the lit_type
						mov_stmt->rhs().rt = ex_rtype(lit_type);
						// Change the target of the mov to tgt
						mov_stmt->lhs() = addr_ref(tgt);
						
						DUMP_T std::cout << "simplified a useless cast" << std::endl;
					}

					s->make_nop();
				}
			}
	});

	// Regenerate the access info so it is up to date.
	// Also folow copies so that something like
	//
	// mov r0 1
	// mov r1 r0
	// ... ... r1
	//
	// becomes
	// ... ... 1
	info = access_info(*optimizing, true, true);

	// Now, lets remove any reads to a register where the ony source of the register is a mov where the rhs is a literal
        if (do_replace_literal_params()) traverse_f(optimizing->start, [&](statement *s){
			if (si_addrof(*s)) return;
			int i = 0;
			s->for_all_write([&](auto){++i;});
			s->for_all_read([&](auto &reg){
				int index = i++;

				// First, check the sources of this parameter.
				auto &d = info.data[s].parameters[index];

				if (d.size() == 0) return; // Must be at least one source
				if (!std::all_of(d.begin(), d.end(), [&](auto &source){return std::holds_alternative<statement *>(source);})) return; // Must all be statements
				long last = ~0;
				if (!std::all_of(d.begin(), d.end(), [&](auto &source){
							// Make sure that a) this is a mov and b) it's arg is a literal and c) the literal is equal for the first statement
							statement * s2 = *std::get_if<statement *>(&source);
							if (si_fcall(*s) && si_mov(*s2) && ai_num(s2->rhs()) && s2->rhs().rt.ptr != nullptr) return false;
							return si_mov(*s2) && ai_num(s2->rhs()) && s2->rhs().num == (last == ~0 ? (last = s2->rhs().num) : last);
				})) return;
				// OK! The only sources of this register all agree on one constant literal.
				// Replace our parameter with that literal.
				reg = addr_ref(ar_type::num, last);
				DUMP_T std::cout << "replaced literal parameter" << std::endl;
				++modifications;
			});
	});

	info = access_info(*optimizing, true, true); // Now, lets do it all again but with ifnz following
	if (do_replace_ifnz_literals()) traverse_f(optimizing->start, [&](statement *s){
			int i = 0;
			s->for_all_write([&](auto){ ++i; });
			s->for_all_read([&](auto &reg){
					int index = i++;

					auto &d = info.data[s].parameters[index];

					if (d.size() != 1) return; // For simplicity, we only handle checking for one source, an ifnz statement
					if (!std::all_of(d.begin(), d.end(), [&](auto &source){return std::holds_alternative<statement *>(source);})) return; // Must all be statements. This also weeds out ai_num && ai_ident
					bool is_zero = false;
					statement * src = nullptr;
					if (!std::all_of(d.begin(), d.end(), [&](auto &source){
								// Make sure that a) this is an if and b) it's arg is a literal and c) the literal is equal for the first statement
								src = *std::get_if<statement *>(&source);
								return si_ifnz(*src) && /* is the statement reachable from either next or cond but not both */ (( is_zero = reachable(src->next, s) ) != reachable(src->cond, s))
								                    && /* the register compared in the ifnz is equal to reg */ reg == src->lhs();
					})) return;
					// OK! The value is at least known to be != 0 or == 0.
					// If the statement is an ifnz, we can just put in a constant here.
					if (si_ifnz(*s)) {
						++modifications;
						DUMP_T std::cout << "replaced guaranteed value from ifnz" << std::endl;
						s->lhs() = addr_ref(ar_type::num, static_cast<long>(!is_zero));
					}
					else {
						// Alright fine, we have to check if we can guarantee the value will be 0 or 1. This means checking if the source of
						// the condition in the ifnz comes from something else
						auto l = info.data[src].parameters[0].begin();
						if (std::holds_alternative<statement *>(*l)) {
							auto s3 = *std::get_if<statement *>(&*l); // Necessary for ultra hacky reasoning
							if (si_gt(*s3) || si_eq(*s3)) {
								// Alright, we can use the same trick we did above
								++modifications;
								DUMP_T std::cout << "replaced guaranteed value from gt/eq->ifnz" << std::endl;
								reg = addr_ref(ar_type::num, static_cast<long>(!is_zero));
							}
						}
					}
			});

			// Try the other strategy, which is to eliminate constructs like
			// if (something) something
			// if (something) something_else
			// something_also_else
			//
			// where s is the first conditional's action
			//
			// This can be optimized if:
			if (s->next != nullptr && si_if(*s->next)) {
				// Keep an easyref
				const auto &d = info.data[s->next];
				statement * src{};

				// Make sure all of its params are the same at both points
				if (std::all_of(s->next->params.begin(), s->next->params.end(), [&, i=0] (const addr_ref& param) mutable -> bool {
					if (!std::all_of(d.parameters[i].begin(), d.parameters[i].end(),
								[&](auto &source){return std::holds_alternative<statement *>(source);})) return false; // Must all be statements. This also weeds out ai_num && ai_ident
					if (d.parameters[i].size() != 1) return false;
					statement * src_of_this = std::get<statement *>(*d.parameters[i].begin());
					if (src && src_of_this != src) return false;
					src = src_of_this;


					// Is the source at src the same source
					if (src->t != s->next->t) return false;
					// Are the parameters equal
					if (src->params[i] != param) return false;

					if (d.parameters[i] != info.data[src].parameters[i]) return false;
					DUMP_T std::cout << "replaced adjacent equal conditions maybe" << std::endl;

					// Ok, this is good so far
					++i;
					return true;
				})) {
					DUMP_T std::cout << "replaced adjacent equal conditions maybe" << std::endl;
					// Make sure the flow makes sense given the diagram
					if (src->next == s->next && reachable(src->cond, s) && !reachable(src->next, s)) {
						// Alright, we can thread s->next into s->next->cond
						++modifications;
						DUMP_T std::cout << "replaced adjacent equal conditions" << std::endl;
						s->next = s->next->cond;
					}
				}
			}
	});

	// Alright. Now for actual copy elision
	//
	// Copy elision is when a statement reads from a parameter who's only source is a copy statement.
	// There can be reads of the source to the copy statement, but the source of the copy statement must be the same
	// as the presence array at the point of the instruction.
	//
	// It is _also_ when a statement writes to a register which is only read by movs.
	// The movs must all write to the same register. They also must all only read from the original statement.
	
	info = access_info(*optimizing, false, false);

	// Go through all statements. See where they read from
	traverse_f(optimizing->start, [&](statement *s){
		// Where does this statement read values from?
		
		int i = 0;
		s->for_all_write([&](auto &reg){
			int index = i++;

			// Alright. Time to _fight to the death_.
			// First, we need to ensure all readers are movs.
			if (!do_copy_elide_write()) return;
			
			auto &d = info.data[s].parameters[index];
			if (!std::all_of(d.begin(), d.end(), [&](auto &src){return std::holds_alternative<statement *>(src);})) return;
			std::vector<statement *> readers;
			for (auto &e : d) {
				readers.push_back(*std::get_if<statement *>(&e));
			}
			if (readers.size() == 0) return;
			// Now we have to make sure they are all movs
			if (!std::all_of(readers.begin(), readers.end(), [](auto &s){return si_mov(*s);})) return;

			// Now, we need to make sure each mov writes to the same value
			addr_ref compare = readers[0]->lhs();
			if (ai_num(compare)) return;
			if (!std::all_of(readers.begin(), readers.end(), [&](auto &r){return r->lhs() == compare;})) return;
			
			// Record the first reader's values for compare to check they all hold the same value.
			auto compareval = info.data[s].everything[compare.num];

			// Alright, we may be ok. The last thing we need to check is that all of these readers' sources are equal to s
			for (auto &r : readers) {
				auto sources = info.data[r].parameters[1];
				if (sources.size() != 1) return;
				if (info.data[r].everything[compare.num] != compareval) return;
				for (auto source : sources) {
					if (!std::holds_alternative<statement *>(source)) return;
					if (std::get<statement *>(source) != s) return;
				}
				for (auto &potential_block : compareval) {
					if (!std::holds_alternative<statement *>(potential_block)) return;
					auto potential_bstmt = std::get<statement *>(potential_block);
					if (potential_bstmt == s) continue;
					if (reachable_before(s, potential_bstmt, r)) return;
				}
			}

			// Alright. We are good to go.
			DUMP_T std::cout << "copy elision (type 2, write) " << reg.num << "->" << compare.num << std::endl;
			//print_statement_list(optimizing->start);
			reg = compare;
			for (auto &r : readers) r->make_nop();
			//print_statement_list(optimizing->start);
			++modifications;
		});

		s->for_all_read([&](auto &reg) {
			int index = i++;
			if (!do_copy_elide_read()) return;
			if (!ai_reg(reg)) return; // Ignore literals + idents
			// Ensure all sources of the register are copies where the sources are all equal
			auto &d = info.data[s].parameters[index];
			addr_ref src_r{};
			if (std::all_of(d.begin(), d.end(), [&](auto &src){
						if (!std::holds_alternative<statement *>(src)) return false;
						auto s2 = *std::get_if<statement *>(&src);
						if (!si_mov(*s2)) return false;
						if (!ai_reg(s2->rhs())) return false;
						// Alright, now check the src
						if (s2->rhs() != (src_r.t == ar_type::unknown ? (src_r = s2->rhs()) : src_r)) return false;
						// Alright, the source is equal. Are the sources to the source the same?
						return info.data[s2].parameters[1] == info.data[s].everything[src_r.num];
			})) {
				// OK! At this point, src_r contains a valid source for reg. Replace it now.
				++modifications;
				DUMP_T std::cout << "copy elision (type 1, read): ";
				DUMP_T std::cout << reg.num;
				DUMP_T std::cout << " to ";
				DUMP_T std::cout << src_r.num;
				DUMP_T std::cout << std::endl;
				reg = src_r;
			}
			
		});
		
	});



	return modifications;
}

int  tacoptimizecontext::optimize_jumpthread() {
	int modifications = 0;

	traverse_f(optimizing->start, [&](statement *s){
			// Check if the pointed at statement is an ifnz, and the next statement is an ifnz, and the parameters are the same
			while (do_jumpthread() && si_if(*s) && s->next != nullptr && si_if(*s->next) && s->t == s->cond->t && s->lhs() == s->next->lhs() && !s->lhs().is_volatile() 
					&& s->rhs() == s->next->rhs() && !s->rhs().is_volatile()
					&& s->next != s->next->next) {
				// next pointer is only executed if condition is false, the condition is always
				// false since lhs is not volatile. this means that the cond pointer is never ran, so the entire statement can be skipped
				// from the first statement
				DUMP_T std::cout << "if threaded" << std::endl;
				++modifications;
				s->next = s->next->next;
			}
			while (do_jumpthread() && si_if(*s) && s->cond != nullptr && si_if(*s->cond) && s->t == s->cond->t && s->lhs() == s->cond->lhs() && !s->lhs().is_volatile()
					&& s->rhs() == s->cond->rhs() && !s->rhs().is_volatile()
					&& s->cond != s->cond->cond) {
				// next pointer is only executed if condition is false, the condition is always
				// false since lhs is not volatile. this means that the cond pointer is never ran, so the entire statement can be skipped
				// from the first statement
				DUMP_T std::cout << "if threaded (cond)" << std::endl;
				++modifications;
				s->cond = s->cond->cond;
			}

			// Check if the ifnz has a literal condition
			while (do_literal_jump_hardcode() && si_ifnz(*s) && ai_num(s->lhs())) {
				// hardcode the jump
				s->next = s->lhs().num ? s->cond : s->next;
				s->make_nop();
				DUMP_T std::cout << "literal ifnz hardcoded (type 1)" << std::endl;
				++modifications;
			}
			while (do_literal_jump_hardcode() && si_ifeq(*s) && s->lhs() == s->rhs() && !s->lhs().is_volatile()) {
				// hardcode the jump
				s->next = s->cond;
				s->make_nop();
				DUMP_T std::cout << "literal ifeq hardcoded (type 1)" << std::endl;
				++modifications;
			}
			while (do_literal_jump_hardcode() && si_ifgt(*s) && ai_num(s->lhs()) && ai_num(s->rhs())) {
				// hardcode the jump
				s->next = (s->lhs().num > s->rhs().num) ? s->cond : s->next;
				s->make_nop();
				DUMP_T std::cout << "literal ifgt hardcoded (type 1)" << std::endl;
				++modifications;
			}

			while (do_remove_redundant_ifnz() && si_if(*s) && s->next == s->cond) {
				s->make_nop();
				DUMP_T std::cout << "redundant if removed" << std::endl;
				++modifications;
			}

			while (do_literal_jump_hardcode() && (si_mov(*s) || si_cast(*s)) && s->next != nullptr && si_ifnz(*s->next) && ai_num(s->rhs()) && s->lhs() == s->next->lhs()) {
				// hardcode our next with the result of of the ifnz
				s->next = s->rhs().num ? s->next->cond : s->next->next;
				DUMP_T std::cout << "literal ifnz hardcoded (type 2)" << std::endl;
				++modifications;
			}

			if (si_mov(*s) && s->next != nullptr && si_ret(*s->next) && s->lhs() == s->next->lhs()) {
				s->reinit(st_type::ret, s->rhs());
				DUMP_T std::cout << "elided copy->ret" << std::endl;
				++modifications;
			}

			if (si_fcall(*s) && s->next != nullptr && si_ret(*s->next) && s->lhs() == s->next->lhs()) {
				// Make sure the thing being called is an ident of this function
				addr_ref &tgt = s->params[1];
				if (do_remove_tail_call() && ai_ident(tgt) && tgt.ident.type == id_type::function && tgt.ident.name == optimizing_name && s->params.size() == 2) {
					// Ok, thread back into main
					
					// But first, we have to move params around
					// TODO: param shuffling, right now it only works if there are no params
					s->make_nop();
					s->next = optimizing->start;
					++modifications;
					DUMP_T std::cout << "tail call removed" << std::endl;
				}
			}

	});

	return modifications;
}

int  tacoptimizecontext::optimize_simplify() {
	int modifications = 0;

	// Traverse the entire tree
	// Since literals should have been replaced inside copy elision, we don't actually need to check for them explicitly here
	// The only case we could do that in is if we wanted to check for ifnzs, which although supported in flow.h will probably not
	// be added here for a while.
	//
	// Some optimizations are already done in the deadcode
	
	auto info = access_info(*optimizing, false, false);
	
	traverse_f(optimizing->start, [&](statement *s){
		switch (s->t) {
			case st_type::add:
				// Are all parameters literal?
			{
				if (!do_tac_arith_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Ok, replace this statement with a literal mov
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								(s->params[1].num + s->params[2].num) // & ((1llu<<size_of_rtype(s->lhs().rt))-1)
					));
					++modifications;
					DUMP_T std::cout << "replaced add with literal params" << std::endl;
					break; // This is already done.
				}

				// Are any of the parameters 0?
				if (std::any_of(++s->params.begin(), s->params.end(), [](auto &reg){return ai_num(reg) && reg.num == 0;})){
					// Get the parameter which is not 0
					auto it = std::find_if_not(++s->params.begin(), s->params.end(), [](auto &reg){return ai_num(reg) && reg.num == 0;});
					if (it == s->params.end()) break;
					auto &other = *it;
					// Check if the expression was an augassin
					if (s->lhs() == other) {
						s->make_nop();
						DUMP_T std::cout << "replaced redundant add with nop" << std::endl;
						++modifications;
					}
					else {
						// Replace with a mov
						s->reinit(st_type::mov, s->lhs(), other);
						DUMP_T std::cout << "replaced add+0 with mov" << std::endl;
						++modifications;
					}
					break;
				}
				break;
			}
			case st_type::mul:
				// Are all parameters literal?
			{
				if (!do_tac_arith_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Ok, replace this statement with a literal mov
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								(s->params[1].num * s->params[2].num) // & ((1llu<<size_of_rtype(s->lhs().rt))-1)
					));
					++modifications;
					DUMP_T std::cout << "replaced mul with literal params" << std::endl;
					break; // This is already done.
				}

				// Are any of the parameters 1?
				if (std::any_of(++s->params.begin(), s->params.end(), [](auto &reg){return ai_num(reg) && reg.num == 1;})){
					// Get the parameter which is not 1
					auto it = std::find_if_not(++s->params.begin(), s->params.end(), [](auto &reg){return ai_num(reg) && reg.num == 1;});
					if (it == s->params.end()) break;
					auto &other = *it;
					// Check if the expression was an augassin
					if (s->lhs() == other) {
						s->make_nop();
						DUMP_T std::cout << "replaced redundant mul with nop" << std::endl;
						++modifications;
					}
					else {
						// Replace with a mov
						s->reinit(st_type::mov, s->lhs(), other);
						DUMP_T std::cout << "replaced mul*1 with mov" << std::endl;
						++modifications;
					}
					break;
				}
				break;
			}
			case st_type::div:
			{
				if (!do_tac_arith_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Ok, replace this statement with a literal mov
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								s->params[1].num / s->params[2].num & (1<<size_of_rtype(s->lhs().rt))-1
					));
					++modifications;
					DUMP_T std::cout << "replaced div with literal params" << std::endl;
				}
				break;
			}
			case st_type::mod:
			{
				if (!do_tac_arith_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Ok, replace this statement with a literal mov
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								s->params[1].num % s->params[2].num & (1<<size_of_rtype(s->lhs().rt))-1
					));
					++modifications;
					DUMP_T std::cout << "replaced mod with literal params" << std::endl;
				}
				if (ai_num(s->rhs()) && s->rhs().num == 1l) {
					// this can be replaced with zero
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 0));
					++modifications;
					DUMP_T std::cout << "replaced mod % 1 with zero" << std::endl;
				}
				break;
			}
			case st_type::neg:
			{
				if (!do_tac_arith_constfold()) break;
				if (ai_num(s->rhs())) {
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, -s->rhs().num));
					++modifications;
					DUMP_T std::cout << "replaced neg with literal to -literal" << std::endl;
				}
				break;
			}
			case st_type::eq:
			{
				if (!do_tac_logic_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Alright. We can replace this with a constant.
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								static_cast<long>(s->params[1].num == s->params[2].num)));
					++modifications;
					DUMP_T std::cout << "replaced eq with constant" << std::endl;

				}
				break;
			}
			case st_type::gt:
			{
				if (!do_tac_logic_constfold()) break;
				if (std::all_of(++s->params.begin(), s->params.end(), ai_num)) {
					// Alright. We can replace this with a constant.
					s->reinit(st_type::mov, s->lhs(), addr_ref(ar_type::num, 
								static_cast<long>(s->params[1].num == s->params[2].num)));
					++modifications;
					DUMP_T std::cout << "replaced eq with constant" << std::endl;

				}
				break;
			}
			case st_type::ifnz:
			{
				// Check if the source of this ifnz is an eq with a literal zero.
				auto &d = info.data[s].parameters[0];
				if (d.size() == 1) {
					auto &source = *d.begin();
					if (auto p = std::get_if<statement *>(&source); p != nullptr) {
						auto stmt_src = *p;
						if (si_eq(*stmt_src)) {
							// Try to make an ifeq
							
							// Check if all operands to the EQ are still accessible at the time of the ifnz
							if (info.data[stmt_src].parameters[0].size() == 1 && std::all_of(++stmt_src->params.begin(), stmt_src->params.end(), [&,i=1](auto &rf) mutable {
								int other = i++;
								return !ai_reg(rf) || info.data[stmt_src].parameters[other] == info.data[s].everything[rf.num];
							}) && do_canonize_ifnz()) {
								// Alright, we can create an ifeq
								s->reinit(st_type::ifeq, stmt_src->params[1], stmt_src->params[2]);
								stmt_src->make_nop();
								++modifications;
								DUMP_T std::cout << "replaced if+eq with ifeq" << std::endl;
							}
						}
						else if (si_gt(*stmt_src) && do_canonize_ifnz()) {
							// Try to create a ifgt
							// Check if all operands to the gt are still accessible at the time of the ifnz
							if (info.data[stmt_src].parameters[0].size() == 1 && std::all_of(++stmt_src->params.begin(), stmt_src->params.end(), [&,i=1](auto &rf) mutable {
								int other = i++;
								return !ai_reg(rf) || info.data[stmt_src].parameters[other] == info.data[s].everything[rf.num];
							})) {
								// Alright, we can create an ifeq
								s->reinit(st_type::ifgt, stmt_src->params[1], stmt_src->params[2]);
								stmt_src->make_nop();
								++modifications;
								DUMP_T std::cout << "replaced if+gt with ifgt" << std::endl;							
							}
						}
					}
				}
				break;
			}
			case st_type::ifeq:
			{
				// Check if one of our parameters is a zero. If so, replace with a ifnz with cond and next swapped.
				int other;
				if (std::count_if(s->params.begin(), s->params.end(), [&,i=0](auto &rf) mutable {
							int index = i++;
							if (!ai_num(rf)) { other = index; return false; }
							return rf.num == 0;
				}) == 1) {
					addr_ref condition = s->params[other];
					s->reinit(st_type::ifnz, std::move(condition));
					std::swap(s->next, s->cond);
					++modifications;
					DUMP_T std::cout << "replaced ifeq == 0 with !ifnz" << std::endl;
				}

				break;
			}
			case st_type::mov:
			{
				if (!do_remove_useless_mov()) break;
				// Replace
				// mov r0 r0
				// with nothing
				if (s->lhs() == s->rhs()) {
					s->make_nop();
					++modifications;
					DUMP_T std::cout << "replaced mov x=x" << std::endl;
				}
			}
			case st_type::fcall:
			{
				if (!do_remove_fcall_result_cast()) break;
				// Replace
				// fcall r1 ....
				// cast r1 r1
				// with
				// fcall r1 ....
				if (s->next != nullptr && si_cast(*s->next)) {
					if (s->lhs() == s->next->rhs()) {
						if (ai_reg(s->lhs()) && ai_reg(s->next->lhs()) && s->next->lhs().num == s->next->rhs().num) {
							s->lhs() = s->next->lhs();
							s->next->make_nop();
							++modifications;
							DUMP_T std::cout << "removed useless fcall result cast" << std::endl;
						}
					}
				}
			}
			default:
				break;
		}
	});

	return modifications;
}

int tacoptimizecontext::optimize_rename() {
	int modifications = 0;
	if (!do_rename_registers()) return 0;

	// Alright. Matthew's Renaming Algorithm (tm) consists of the following:
	//
	// Go through all statements. If they don't write to anything, skip.
	std::vector<statement *> traversal = traverse_v(optimizing->start);
	traversal.erase(std::remove_if(traversal.begin(), traversal.end(), [&](auto &s){
		int i = 0;
		s->for_all_write([&](auto){++i;});
		return i == 0;
	}), traversal.end());

	auto info = access_info(*optimizing, false, false); // Follow absolutely nothing.

	for (auto &writer : traversal) {
		{
			// First, lets find anything else that is a possible reader, and collect all possible sources associated with this writer.
			// In order for a writer to be valid, its sources must be equal to our sources, at least up to (but not including) the replaced number
			// We also must be replacing a register.
			if (!ai_reg(writer->lhs())) continue; // Not a register write command
			// Alright, now lets make a copy of a few things
			auto &lore = info.data[writer];
			std::list<statement *> to_investigate;
			std::set<source_type>  validated_sources;
			std::set<statement *>  checkable{};
			std::list<int>          consider{};
			for (auto i = optimizing->num_params; i < writer->lhs().num; ++i) {
				consider.push_back(i);
			}
			if (consider.size() == 0) continue; // No possible improvement
			to_investigate.push_back(writer);
			while (!to_investigate.empty()) {
				auto investigating = to_investigate.front();
				to_investigate.pop_front();

				if (!ai_reg(investigating->lhs())) goto break_loop; // Not a register write command
				auto &their_lore = info.data[investigating];
				// First, make sure the everything values are good.
				if (!std::equal(lore.everything.begin(), lore.everything.begin() + writer->lhs().num,
						their_lore.everything.begin(), their_lore.everything.begin() + investigating->lhs().num)) {
					goto break_loop; // this is not ok
				}
				checkable.insert(investigating);
				// Alright, this might be ok. Now we can check the dependents
				for (auto &dependent : their_lore.parameters[0]) {
					// This _must_ be a statement, if it isn't something has gone horribly wrong
					auto depend = *std::get_if<statement *>(&dependent);
					if (si_addrof(*depend)) goto break_loop;
					// Check where the depend got its value from:
					if (checkable.insert(depend).second) {
						int i = 0;
						depend->for_all_write([&](auto &reg){
								int index = i++;
								// Additionally, check if this statement writes to the same register. This would mean we have
								// to check it too.
								if (depend != writer && ai_reg(reg) && reg.num == writer->lhs().num && validated_sources.insert(depend).second) to_investigate.push_back(depend);
						});
						for (;i < depend->params.size(); ++i) {
							auto &reg = depend->params[i];
							if (reg != writer->params[0]) continue;
							for (auto &src : info.data[depend].parameters[i]) {
								if (std::holds_alternative<statement *>(src)) {
									auto source = *std::get_if<statement *>(&src);
									if (validated_sources.insert(source).second) {
										to_investigate.push_back(source);
									}
								}
								else {
									// Invalid
									goto break_loop;
								}
							}
						}
						// Considerable values now need filtering
						for (auto i = consider.begin(); i != consider.end();) {
							if (info.data[depend].everything[*i] != lore.everything[*i]) {
								i = consider.erase(i);	
							}
							else {
								++i;
							}
						}
					}
				}
			}

			// Ok. At this point, consider contains a list of potential renames and checkable contains every single statement where the renamed value
			// 's readers must not be accessible.
			
			for (auto &i : consider) {
				{
					// Test the active source at this time. If it's undefined, use it.
					auto &d = lore.everything[i];
					if (std::all_of(d.begin(), d.end(), [&](auto &c_source){
								if (std::holds_alternative<undefined_source>(c_source)) return true;
								// Otherwise, check what the active source of the register is read by
								// If we can reach it, then bad things are going to happen
								else if (std::holds_alternative<statement *>(c_source)) {
									if (si_addrof(*std::get<statement *>(c_source))) return false;  // this has been written to in a SPECIAAAAAAAAAL WAY
									auto &dd = info.data[*std::get_if<statement *>(&c_source)].parameters[0];
									return std::all_of(dd.begin(), dd.end(), [&](auto reader){
											return std::all_of(checkable.begin(), checkable.end(), [&](auto &beginning){
													const auto stmt_read = *std::get_if<statement *>(&reader);
													if (si_addrof(*stmt_read)) return false;
													// Can we get to the source before we get to the reader starting at where we want to clobber this register?
													if (reachable_before(beginning, std::get<statement *>(c_source), stmt_read)) return true;
													return false;
											});
									});
								}
								else {
									return false;
								}
					})) {
						// OK! Valid. Rename original to i
						int original = writer->lhs().num;
						auto rename = [&](addr_ref &reg) {
							if (ai_reg(reg) && reg.num == original) {
								reg.num = i;
							}
						};
						for (auto &renamer : checkable) {
							renamer->for_all_write(rename);
							renamer->for_all_read(rename);
						}
						DUMP_T std::cout << "renamed " << original << " to " << i << std::endl;
						++modifications;
						return modifications;
					}
					else {
						DUMP_T std::cout << "cancel rename " << writer->lhs().num << " to " << i << std::endl;
					}
				}
			}
		}
		break_loop:;
	}

	return modifications;
}

void tacoptimizecontext::remove_register_holes() {
	// This function walks through the entire tree, working out which registers are never written to. It then goes through the tree again, but
	// modifies all references to any hole+1 to the hole by looking it up in a map.
	if (!do_remap_registers()) return;
	
	int remaps;
	do {
		remaps = 0;
		std::set<int> regnums;
		
		auto traversal = traverse_v(optimizing->start);
		for (auto &stmt : traversal) {
			for (auto &param : stmt->params) {
				if (!ai_reg(param)) continue;
				regnums.insert(param.num);
			}
		}

		// Next, we make a map of targets.
		std::map<int, int> remapping;
		for (auto &i : regnums) {
			if (regnums.count(i-1) || i == 0) continue;
			int j = i;
			int k = i;
			while (!regnums.count(k-1) && k != 0) --k; 
			remapping[j] = k;
			++remaps;
			DUMP_T std::cout << "remapping " << j << " to " << k << std::endl;
		}

		// Now, remap all uses of k to v in remapping
		for (auto &stmt : traversal) {
			for (auto &param : stmt->params) {
				if (!ai_reg(param)) continue;
				if (!remapping.count(param.num)) continue;
				param.num = remapping[param.num];
			}
		}
	} while (remaps);
}

int tacoptimizecontext::optimize_canonize() {
	int modifications = 0;
	traverse_f(optimizing->start, [&](statement *&s){
		if (s->commutative()) {
			// Find its first non-write param.
			int i = 0; s->for_all_write([&](auto &){i = 1;});
			// Check if they're sorted.
			auto compare = [](const addr_ref& a, const addr_ref& b){
				if (a.t != b.t) return a.t < b.t;
				if (ai_reg(a) || ai_reg(b)) return a.num < b.num;
				return a.ident < b.ident;
			};

			if (!std::is_sorted(++s->params.begin(), s->params.end(), compare)) {
				DUMP_T std::cout << "recanonized" << std::endl;
				std::sort(++s->params.begin(), s->params.end(), compare);
				++modifications;
			}
		}
	});
	return modifications;
}

int tacoptimizecontext::optimize_stackoff() {
	// Find all adds that reference a stackoff
	int modifications = 0;
	
	auto info = access_info(*optimizing, false, false); // this might be a true, fales i have to debate

	traverse_f(optimizing->start, [&](statement *&s){
		if (!si_add(*s)) return;
		// check if it is of the form reg + const
		if (!(ai_reg(s->params[1]) && ai_num(s->rhs()))) return;

		// Where did the reg come from?
		// (and where did it go)
		
		auto sources = info.data[s].parameters[1];

		if (sources.size() != 1 || !std::holds_alternative<statement *>(*sources.begin())) return;

		auto src = std::get<statement *>(*sources.begin());

		// Is the source a stackoffset?
		if (!si_stackoff(*src)) return;

		// Is the rhs constant?
		
		if (!ai_num(src->rhs())) return;

		// Eliminate this add
		long new_offset = src->rhs().num + s->rhs().num;

		s->reinit(st_type::stackoff, s->lhs(), addr_ref{ar_type::num, new_offset});
		++modifications;
		std::cout << "replaced add from stackoff" << std::endl;
	});
	return modifications;
}

#undef DUMP_T
