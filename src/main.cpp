#include <iostream>
#include <fstream>
#include <parser.h>
#include "stringify.h"
#include "ast_optimize.h"
#include "tac_optimize.h"
#include "compiler.h"
#include "codegen.h"

int main(int argc, char ** argv) {
	arg_info a = parseargs(argc, argv);
	std::string filename = a.input_name;
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
	debug_dump_ctx(ctx);

	astoptimizecontext ast_octx(std::move(ctx));
	ast_octx.optimize();

	debug_dump_ctx(ast_octx);

	compiler comp(std::move(ast_octx));
	comp.compile_all();

	debug_dump_ctx(comp);

	tacoptimizecontext tac_octx(std::move(comp));
	tac_octx.optimize();

	debug_dump_ctx(tac_octx);

	if (arch == "x86_64") {
		x86_64::codegenerator cg(std::move(tac_octx));
		cg.generate();
		
		debug_dump_ctx(cg);

		// Write output to a file
		std::ofstream output(a.output_name);
		output << ".bss" << std::endl << cg.bss_section << std::endl;
		output << ".text" << std::endl << cg.text_section << std::endl;
		output << ".globl main" << std::endl;
		output << ".data" << std::endl;
		for (int i = 0; i != tac_octx.string_table.size(); ++i) {
			if (tac_octx.string_table[i] == '\0') {
				tac_octx.string_table[i] = '0';
				tac_octx.string_table.insert(tac_octx.string_table.begin()+i, '\\');
			}
		}
		output << "___strtable: .ascii \"" << tac_octx.string_table << "\"" << std::endl;
		
		output.flush();
		output.close();
	}
	return 0;
}
