#include <iostream>
#include <fstream>
#include <parser.h>
#include "stringify.h"
#include "ast_optimize.h"
#include "tac_optimize.h"
#include "compiler.h"

int main(int argc, char ** argv) {
	std::cout << "mcc version 0.1" << std::endl;
	std::string filename = argv[1];
	std::ifstream f(filename);
	std::string buffer(std::istreambuf_iterator<char>(f), {});
	
	// setup the parser context
	parsecontext ctx;
	ctx.cursor = buffer.c_str();
	ctx.loc.begin.filename = &filename;
	ctx.loc.end.filename   = &filename;

	// create the parser object
	yy::mlang_parser parser(ctx);
	parser.parse(); // parse the code from the file

	// debug printing: first print defined globals
#ifndef NDEBUG
	debug_dump_ctx(ctx);
#endif

	astoptimizecontext ast_octx(std::move(ctx));
	ast_octx.optimize();

#ifndef NDEBUG
	debug_dump_ctx(ast_octx);
#endif

	compiler comp(std::move(ast_octx));
	comp.compile_all();

#ifndef NDEBUG
	debug_dump_ctx(comp);
#endif

	tacoptimizecontext tac_octx(std::move(comp));
	tac_octx.optimize();

#ifndef NDEBUG
	debug_dump_ctx(tac_octx);
#endif
	return 0;
}
