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

	static std::string reg_name_to_str(const reg_name &r) {
		switch (r) {
			case reg_name::a:
				return "a";
			case reg_name::b:
				return "b";
			case reg_name::c:
				return "c";
			case reg_name::d:
				return "d";
			case reg_name::si:
				return "si";
			case reg_name::di:
				return "di";
			case reg_name::r8:
				return "r8";
			case reg_name::r9:
				return "r9";
			case reg_name::r10:
				return "r10";
			case reg_name::r11:
				return "r11";
			case reg_name::r12:
				return "r12";
			case reg_name::r13:
				return "r13";
			case reg_name::r14:
				return "r15";
			case reg_name::r15:
				return "r15";

		}
	}
	
	struct reg {
		reg_name name;
		int size = 4;

		reg(reg_name rn) : name(rn) {}
		reg(reg_name rn, int size) : name(rn) {this->size = size;}

		bool operator==(const reg& other) const {
			return name == other.name && size == other.size;
		}

		bool operator!=(const reg& other) const {
			return !operator==(other);
		}

		std::string to_string() const {
			if (name >= reg_name::r8) {
				std::string base = reg_name_to_str(name);
				if (size == 4) return base;
				else return base + " bwd"[size];
			}
			else {
				if (size == 1 && name >= reg_name::si) {
					return reg_name_to_str(name) + "l";
				}
				else if (size == 4 && name >= reg_name::si) {
					return "r" + reg_name_to_str(name);
				}
				else if (name >= reg_name::si) {
					return reg_name_to_str(name); // will cause assembler error
				}
				else {
					if (size >= 3) {
						return "   er"[size] + reg_name_to_str(name) + "x";
					}
					else if (size == 2) return reg_name_to_str(name) + "x";
					else return reg_name_to_str(name) + "l";
				}
			}
		}
	};

	// int is a stack offset number.
	// all registers which get allocated onto the stack are created 
	//
	// int_16 is our stack offset
	// long is an immediate value
	typedef std::variant<reg, std::string, int16_t, long> param_type;

	// Converts a parameter to a memory indexed operand, immediate or a register name
	static std::string nameoffset_to_str(const param_type &p) {
		if (std::holds_alternative<int16_t>(p)) {
			return std::to_string(std::get<int16_t>(p)) + "(%rbp)";
		}
		else if (std::holds_alternative<std::string>(p)) {
			return std::get<std::string>(p);
		}
		else if (std::holds_alternative<reg>(p)) {
			return "%" + std::get<reg>(p).to_string();
		}
		else {
			return "$" + std::to_string(std::get<long>(p));
		}
	}

	static inline std::string ssuffix(int size) {
		std::string re{};
		switch (size) {
			case 1:
				re += 'b';
				break;
			case 2:
				re += 'w';
				break;
			case 3:
				re += 'l';
				break;
			case 4:
				re += 'q';
				break;
			default:
				break;
		}
		return re;
	}

	struct codegenerator {
		void generate();
		void prepare();
	};
}

#endif