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
			SAMEAS
		};

		std::set<type> valid_types;
		std::set<p_size> valid_sizes;
		int parm;
	};

	const char * registers[4][14] = {
		{"al"  , "bl"  , "cl"  , "dl"  , "sil" , "dil" , "r8b" , "r9b" , "r10b" , "r11b" , "r12b" , "r13b" , "r14b" , "r15b"},
		{"ax"  , "bx"  , "cx"  , "dx"  , "si"  , "di"  , "r8w" , "r9w" , "r10w" , "r11w" , "r12w" , "r13w" , "r14w" , "r15w"},
		{"eax" , "ebx" , "ecx" , "edx" , "esi" , "edi" , "r8d" , "r9d" , "r10d" , "r11d" , "r12d" , "r13d" , "r14d" , "r15d"},
		{"rax" , "rbx" , "rcx" , "rdx" , "rsi" , "rdi" , "r8"  , "r9"  , "r10"  , "r11"  , "r12"  , "r13"  , "r14"  , "r15"} 
	};

	struct recipe {
		int priority;
		st_type type;
		std::string pattern;
		

	};

	struct codegenerator {
	};
}

#endif
