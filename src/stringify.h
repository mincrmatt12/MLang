// stringify.h -- convert expressions to a format that looks semi decent
// prints out tree expressions
// yay


#ifndef STRINGIFY_H
#define STRINGIFY_H

#include <string>
#include <iostream>
#include <parser.h>

template<typename T>
void val_print(const T &v) {};

template<>
void val_print(const long &v) {std::cout << v;}

template<>
void val_print(const std::string &v) {std::cout << v;} 

template<>
void val_print(const identifier &v) {
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

void print_tree(const expression &e, int depth=0) {
	for (int i = 0; i < depth; i++) {
		std::cout << "| ";
	}
	std::cout << "|- ";
#define o(n) case ex_type::n: std::cout << #n; break;
	switch (e.t) {
		ENUM_EXPRESSIONS(o)
	}
#undef o
	std::cout << " ";
	switch (e.t) {
		case ex_type::string_ref:	val_print(e.strvalue); break;
		case ex_type::literal_number: 	val_print(e.numvalue); break;
		case ex_type::ident: 		val_print(e.ident   ); break;
		default:		  break;
	}
	std::cout << std::endl;
	++depth;
	for (const auto &l : e.params) {
		print_tree(l, depth);
	}
}

void debug_dump_ctx(parsecontext& ctx) {
	std::cout << "=== external functions ===" << std::endl;
	for (const auto &e : ctx.ext_list) {
		std::cout << e.name << "(";
		for (int c = 0; c < e.num_args; c++) std::cout << "P" << c << ",";
		if (e.varargs) std::cout << "...)";
		else std::cout << ")";
		std::cout << std::endl << std::endl;
	}

	std::cout << "=== global vars & inits ===" << std::endl;
	for (const auto &[k, v] : ctx.global_ids) {
		if (v.type != id_type::global_var) continue;
		val_print(v); std::cout << ": " << std::endl;
		if (ctx.global_initializers.count(k) != 0) {
			print_tree(ctx.global_initializers[k]);
			std::cout << std::endl;
		}
	}

	std::cout << "=== functions ===" << std::endl;
	for (const auto &f : ctx.func_list) {
		std::cout << f.name << "(";
		for (int c = 0; c < f.num_args; c++) std::cout << "P" << c << ",";
		std::cout << "): " << std::endl;
		std::cout << "= globals =" << std::endl;
		for (int i = 0; i < f.num_globals; i++) {
			std::cout << "$G" << i << ": " << std::endl;
			print_tree(f.global_initializers[i]);
		}
		std::cout << "= code =" << std::endl;
		print_tree(f.code);
	}
}

#endif
