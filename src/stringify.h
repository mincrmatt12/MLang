// stringify.h -- convert expressions to a format that looks semi decent
// prints out tree expressions
// yay


#ifndef STRINGIFY_H
#define STRINGIFY_H

#include <string>
#include <iostream>
#include <parser.h>
#include "ast_optimize.h"
#include "compiler.h"
#include "tac_optimize.h"
#include "args.h"
#include "flow.h"

struct tree_toggle_t {
	bool yes;
	tree_toggle_t * first;
	tree_toggle_t * next = nullptr;
};


static void val_print(const long &v) {std::cout << v;}

static void val_print(const std::string &v) {std::cout << v;} 

static void val_print(const identifier &v) {
	switch (v.type) {
		case id_type::undefined: 	std::cout << "$U"; break;
		case id_type::function:  	std::cout << "$F"; break;
		case id_type::extern_function: 	std::cout << "$E"; break;
		case id_type::global_var: 	std::cout << "$G"; break;
		case id_type::local_var: 	std::cout << "$L"; break;
		case id_type::parameter: 	std::cout << "$P"; break;
	}
	std::cout << v.index << "\t(" << v.name << ")";
}

static void val_print(const ex_rtype &v) {
	std::cout << "<";
	if (v.ptr != nullptr) {
		val_print(*v.ptr);
		std::cout << "*>";
		return;
	}
	std::cout << v.size << ">";
}

static void print_tree(const expression &e, int depth=0, tree_toggle_t * head=nullptr) {
	if (head == nullptr) {
		head = new tree_toggle_t;
		head->first = head;
		std::cout << "╚═╦══";
	}
	else {
		tree_toggle_t * last = head;
		head = new tree_toggle_t;
		last->next = head;
		head->first = last->first;
		tree_toggle_t * starting = head->first;
		std::cout << "  ";
		for (int i = 0; i < depth-1; i++) {
			std::cout << (starting->yes ? "║ " : "  ");
			starting = starting->next;
		}
		if (starting->yes) {
			if (e.params.size() > 0) {
				std::cout << "╠═╦═";
			}	
			else {
				std::cout << "╠═══";
			}
		}
		else {
			if (e.params.size() > 0) {
				std::cout << "╚═╦═";
			}	
			else {
				std::cout << "╚═══";
			}
		}
	}
#define o(n) case ex_type::n: std::cout << #n; break;
	switch (e.t) {
		ENUM_EXPRESSIONS(o)
	}
#undef o
	std::cout << " ";
	switch (e.t) {
		case ex_type::string_ref:	val_print(e.strvalue ); break;
		case ex_type::literal_number: 	val_print(e.numvalue ); break;
		case ex_type::ident: 		val_print(e.ident    ); break;
		case ex_type::cast:		val_print(e.castvalue); break;
		default:		  break;
	}
	std::cout << '\n';
	++depth;
	int car = e.params.size();
	for (const auto &l : e.params) {
		--car;
		head->yes = car != 0;
		print_tree(l, depth, head);
	}
	delete head;
}

static void debug_dump_ctx(parsecontext& ctx) {
	if (dumplevel == 0) return;
	std::cout << "=== external functions ===" << '\n';
	for (const auto &e : ctx.ext_list) {
		std::cout << e.name << "(";
		for (int c = 0; c < e.num_args; c++) std::cout << "P" << c << ",";
		if (e.varargs) std::cout << "...)";
		else std::cout << ")";
		std::cout << '\n' << '\n';
	}

	std::cout << "=== global vars & inits ===" << '\n';
	std::cout << ctx.num_globals << '\n';
	for (int i = 0; i < ctx.num_globals; i++) {
		std::cout << "$G" << i << ":" << '\n';
		print_tree(ctx.global_initializers[i]);
		std::cout << '\n';
	}

	std::cout << "=== functions ===" << '\n';
	for (const auto &f : ctx.func_list) {
		std::cout << f.name << "(";
		for (int c = 0; c < f.num_args; c++) std::cout << "P" << c << ",";
		std::cout << "): " << '\n';
		std::cout << "= code =" << '\n';
		print_tree(f.code);
	}
}

static void debug_dump_ctx(astoptimizecontext& ctx) {
	if (dumplevel == 0) return;
	std::cout << "=== external functions ===" << '\n';
	for (const auto &e : ctx.ext_functions) {
		std::cout << e.name << "(";
		for (int c = 0; c < e.num_args; c++) std::cout << "P" << c << ",";
		if (e.varargs) std::cout << "...)";
		else std::cout << ")";
		std::cout << '\n' << '\n';
	}

	std::cout << "=== global vars & inits ===" << '\n';
	int count = 0;
	for (const auto &v : ctx.global_inits) {
		std::cout << "$G" << count << ":" << '\n';
		++count;
		print_tree(v);
	}

	std::cout << "=== functions ===" << '\n';
	for (const auto &f : ctx.functions) {
		std::cout << f.name << "(";
		for (int c = 0; c < f.num_args; c++) std::cout << "P" << c << ",";
		std::cout << "): " << '\n';
		std::cout << "= code =" << '\n';
		print_tree(f.code);
	}
}

// Dumping functions for statements

static void val_print(const addr_ref &r) {
	switch (r.t) {
		case ar_type::unknown:
			std::cout << "??";
			break;
		case ar_type::num:
			std::cout << "#" << r.num;
			break;
		case ar_type::reg:
			std::cout << "R" << r.num;
			if (r.has_associated_name) {
				std::cout << "<" << r.associated_name << ">";
			}
			break;
		case ar_type::ident:
			val_print(r.ident);
			break;
	}
	val_print(r.rt);
}

static void val_print(const statement &s) {
#define o(n) case st_type::n: std::cout << #n; break;
	switch (s.t) {
		ENUM_STATEMENTS(o)
	}
#undef o
	for (auto &p : s.params) {
		std::cout << " ";
		val_print(p);
	}
}

static void print_statement_list(statement * start, const access_info *extra_info=nullptr) {
	auto indices = std::map<statement *, int>{}; // Get the canonical order of all of these things.
	auto labels  = std::map<int, int>{};
	auto trav    = std::map<int, statement *>{};

	// Create a reverse lookup for iteration
	traverse_f(start, [&](statement *G, int I){
			indices[G] = I;
			trav   [I] = G;
	});

	auto add_label = [&, l=0](int i) mutable {
		if (labels.count(i) == 0) {
			labels[i] = l++;
		}	
	};

	// Assume nothing requires a label. We only need one if we decide we need to jump there at some point.
	// We only jump somewhere if the index of the next pointer is not equal to the index of the current
	// statement + 1.
	// Since conditional statements ALWAYS jump, we can assume they need labels whenever we see them.
	
	// Step one: determine what needs labels by iterating over everything in the indices map
	for (auto &[index, st] : trav) {
		if (st->cond != nullptr) add_label(indices[st->cond]);
		// Check if the index of the next pointer (if it exists) is equal to 1+index
		if (st->next != nullptr) {
			if (indices[st->next] != index+1) add_label(indices[st->next]);
		}
	}

	// Now, print all of the statements, optionally adding labels and jumps
	for (auto &[index, st] : trav) {
		if (labels.count(index) != 0) {
			std::cout << ".L" << labels[index] << ":" << '\n';
		}
		std::cout << "  ";
		if (extra_info) {
			std::cout << 'S' << index << " :: ";
		}
		val_print(*st);
		if (st->cond != nullptr) {
			std::cout << " .L" << labels[indices[st->cond]];
		}
		if (extra_info && extra_info->data.contains(st)) {
			const auto& dat = extra_info->data.at(st);
			std::cout << "\t";
			/*
			st->for_all_read([&](const addr_ref& arg){
				if (ai_reg(arg)) {
					const auto& srcs = dat.everything[arg.num];
					std::cout << "R" << arg.num << "={";
					for (auto& src : srcs) {
						std::visit([&](auto entry){
							if constexpr (std::is_same_v<decltype(entry), parameter_source>) {
								std::cout << "P" << entry << ",";
							}
							else if constexpr (std::is_same_v<decltype(entry), statement *>) {
								if (indices.contains(entry)) {
									std::cout << "S" << indices.at(entry) << ",";
								}
							}
							else
								std::cout << "?,";
						}, src);
					}
					std::cout << "} ";
				}
			});
			*/

			int k = 0;
			for (const auto& srcs : dat.parameters) {
				if (ai_reg(st->params[k])) {
					std::cout << "R" << st->params[k].num << "={";
					for (auto& src : srcs) {
						std::visit([&](auto entry){
							if constexpr (std::is_same_v<decltype(entry), parameter_source>) {
								std::cout << "P" << entry << ",";
							}
							else if constexpr (std::is_same_v<decltype(entry), statement *>) {
								if (indices.contains(entry)) {
									std::cout << "S" << indices.at(entry) << ",";
								}
							}
							else
								std::cout << "?,";
						}, src);
					}
					std::cout << "} ";
				}
				++k;
			}
		}
		std::cout << '\n';
		if (st->next != nullptr) {
			if (indices[st->next] != index+1) {
				std::cout << "  jmp .L" << labels[indices[st->next]] << '\n';
			}
		}
	}
}

static void debug_dump_ctx(compiler &ctx) {
	if (dumplevel == 0) return;
	// First, print out all of the functions
	
	for (auto &[name, stp] : ctx.func_compileunits) {
		if (stp.start == nullptr) continue;
		std::cout << name << ":" << '\n';
		if (stp.array_block_size) {
			std::cout << "  ; array size = " << stp.array_block_size << '\n';
		}
		print_statement_list(stp.start);
		
	}

	std::cout << "$GLOBAL:" << '\n';
	if (ctx.global_initscope.start != nullptr) print_statement_list(ctx.global_initscope.start);
}

static void debug_dump_ctx(tacoptimizecontext &ctx) {
	if (dumplevel == 0) return;
	// First, print out all of the functions
	
	for (auto &[name, stp] : ctx.func_compileunits) {
		if (stp.start == nullptr) continue;
		std::cout << name << ":" << '\n';
		if (stp.array_block_size) {
			std::cout << "  ; array size = " << stp.array_block_size << '\n';
		}
		auto datums = access_info(stp, false, false);
		print_statement_list(stp.start, &datums);
	}

	std::cout << "$GLOBAL:" << '\n';
	if (ctx.global_initscope.start != nullptr) print_statement_list(ctx.global_initscope.start);
}

#endif
