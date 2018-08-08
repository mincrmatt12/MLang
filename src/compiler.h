// compiler.h - implements the ast to tac conversion

#ifndef COMPILER_H
#define COMPILER_H

#include "tac.h"
#include "ast_optimize.h"
#include <memory>

struct compiler {
	std::map<std::string, compilation_unit> func_compileunits;
	compilation_unit global_initscope;
	std::string string_table;
	// Used so we can pass this around into the next stage
	std::vector<ext_function> ext_list;

	std::vector<std::unique_ptr<statement>> all_statements;

	template<typename ...T>
	inline statement* make_statement(st_type t, T&&... args) {
		return all_statements.emplace_back(new statement(t, std::forward<T>(args)...)).get();
	}

#define o(n) \
	template<typename ...T> \
	inline statement * s_##n(T&&... args) {\
		return make_statement(st_type::n, std::forward<T>(args)...);\
	}
	ENUM_STATEMENTS(o)
#undef o

	compilation_unit *currently_compiling;

	compiler(astoptimizecontext &&ctx);
	
	addr_ref compile                                             (const expression &e);
	// Sets up the currently_compiling reference
	void	 compile_unit                                        (compilation_unit &c);
	// Builds a global string table
	void 	 create_string_table                                 ();
	// Calls compile unit to generate linked lists of statements
	void	 compile_all                                         ();
};

#endif //COMPILER_H
