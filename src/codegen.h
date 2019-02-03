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
	// Define registers.
	// Each register has various names to correspond to the different sizes.
	enum struct reg_name : unsigned char {
		a,
		b,
		c,
		d,
		si,
		di,
		r8,
		r9,
		r10,
		r11,
		r12,
		r13,
		r14,
		r15
	};

	// Define an instruction mechanism, similar to how TAC works, with the linked list.
	struct instruction {

	};

	struct codegenerator {
		void generate();
		void prepare();
	};
}

#endif
