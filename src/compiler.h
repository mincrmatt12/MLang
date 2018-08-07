// compiler.h - implements the ast to tac conversion

#include "tac.h"
#include <memory>

struct compiler {
	std::map<std::string, compilation_unit> func_compileunits;
	compilation_unit global_initscope;

	std::vector<std::unique_ptr<statement>> all_statements;
};
