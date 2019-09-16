// codegen.h - converts compile_units to assembler

// Eventually this may use assembler.h, but for now it is going to output good ol' strings
// to get fed into as and then into ld.
//
//
// My code generator's architecture is of a "recipe" model.
// Any given statement can be converted using any one of some set of recipes.
// Recipes also have clobber lists and other requirements, like storage requirements.
// Any valid recipes for some inputs are sorted first by clobber size, and then
// by size of text.

// Since I would like to be able to swap out generators, each specific one is going to go in a namespace

#ifndef CODEGEN_H
#define CODEGEN_H

#include <string>
#include <variant>
#include <vector>
#include <set>
#include <algorithm>
#include <tuple>
#include "tac.h"
#include "tac_optimize.h"

namespace x86_64 {
	enum struct p_size {
		BYTE,
		WORD,
		DWORD,
		QWORD
	};

	struct match_t {
		enum type {
			REG,
			MEM,
			IMM,
			CONSTIMM,
			SAMEAS,
		};

		std::set<type> valid_types;
		std::set<p_size> valid_sizes;
		int parm;
	};

	extern const char * registers[4][14];
	extern const char * sizes[4];

	struct recipe {
		int cost;
		st_type type;
		std::string pattern;
		
		std::vector<match_t> matches;
		std::vector<int> clobbers{};
	};

	extern const recipe recipes[];

	struct storage {
		enum type_t {
			REG,
			IMM,
			STACKOFFSET,
			GLOBAL
		} type;

		int regno{};
		long imm_or_offset{};
		std::string global{};

		uint8_t size{};

		storage() : type(REG), regno(-1) {}
		storage(int regno, uint8_t t) : type(REG), regno(regno), size(t) {};
		storage(type_t t, long imm) : type(t), imm_or_offset(imm) {
			if (imm < 256) size = 8;
			else if (imm < 65536) size = 16;
			else size = 32;
		}
		storage(type_t t, long imm, uint8_t rt) : type(t), imm_or_offset(imm), size(rt) {};
		storage(std::string g, uint8_t t) : type(GLOBAL), global(g), size(t) {};
		storage(const storage& s, uint8_t si) : type(s.type), regno(s.regno), imm_or_offset(s.imm_or_offset), global(s.global), size(si) {};

		std::string to_string() const;
		std::string to_string(p_size requested_size) const;
		std::string addr_string() const;

		bool matches(const match_t& m) const;
		bool matches(const match_t& m, const std::vector<storage> &others) const;
		p_size get_size() const;

		bool operator==(const storage& other) const;
	};

	struct codegenerator {
		codegenerator(tacoptimizecontext &&ctx);

		std::string generate();
	private:
		std::string generate_prologue();
		// Generates _without_ the name.
		std::string generate_unit(compilation_unit &cu);
		// Generate the string table (although it uses numbers to avoid having to escape things)
		std::string output_string_table(std::string table);
		// Generate the bss section
		std::string output_bss_section();

		// Current compilation unit.
		compilation_unit *current;

		// Current register allocation
		std::vector<storage> stores;
		// Local count
		int local_stack_usage;

		// Find the current storage for an addr_ref
		storage get_storage_for(const addr_ref& ar);
		
		// Allocate storage for all TAC registers
		// This is non-trivial, as we must check if there is an addrof operation anywhere.
		void allocate_stores();

		// Imported from tacoptimizer
		std::map<std::string, compilation_unit> func_compileunits;
		compilation_unit global_initscope;
		std::string string_table;
		std::vector<ext_function> ext_list; // finally gets used!

		// Assemblers
		std::string assemble(statement* stmt, int labelno=0);
		// Seperated for functional clarity -- handles ret and fcall, assemble delegates to this
		std::string assemble_special(statement * stmt, int labelno);

		// Helper functions
		bool is_register_used(int regno);
		int  get_clobber_register(std::set<int> important_registers);
		bool is_clobber_available();
		bool is_clobbered_for(int reg, statement* stmt);
	};
}

#endif
