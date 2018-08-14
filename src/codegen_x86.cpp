#include "codegen.h"

namespace x86_64 {
	codegenerator::codegenerator(tacoptimizecontext &&ctx) {
		func_compileunits = std::move(ctx.func_compileunits);
		global_cu         = std::move(ctx.global_initscope);
		ext_functions     = std::move(ctx.ext_list);
		
		// Define the recipes
		
		// Simple instructions first
		#define ANY {0, 1, 2}
                #define REGISTER 0
		#define OFFSET 2
		#define NAME 1
		#define IMM 3

		#define N(x)  nameoffset_to_str(x)
		#define S(x)  ssuffix(x)

		auto recipe = [&](st_type t, std::vector<std::set<int>> &&params, 
				recipe::result_type (*p)(std::vector<param_type>&, std::vector<int>&)) {
			recipes[t].push_back(x86_64::recipe{params, p});
		};

		recipe(st_type::nop, {}, [](auto &p, auto){
			return recipe::result_type{"nop", {}};
		});

		recipe(st_type::str, {{REGISTER}}, [](auto &p, auto){
			// Because p[0] is a register, we can use rip relative addressing
			// to get the address and place it into the register
			return recipe::result_type{
				"leaq ___strtable(%rip), %" + std::get<reg>(p[0]).to_string(),
				{}
			};
		});

		recipe(st_type::str, {{NAME, OFFSET}}, [](auto &p, auto){
			// Compute the lea parameter. For name it is
			// <name>(%rip) to use rip relative accessing (PIC)
			// For offset it is
			// <offset>(%rbp)
			return recipe::result_type{
				"leaq ___strtable(%rip), %r15\nmov %r15, " + N(p[0]),
				{reg_name::r15}
			};
		});

		recipe(st_type::mov, {{REGISTER}, {IMM}}, [](auto &p, auto &s){
			// if the imm is zero use xor
			if (std::get<long>(p[1]) == 0) {
				return recipe::result_type{
					"xor" + S(s[0]) + " " + N(p[0]) + ", " + N(p[0]),
					{}
				};
			}
			else {
				return recipe::result_type{
					"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
					{}
				};
			}
		});
		recipe(st_type::mov, {{REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]),
				{}
			};
		});
		recipe(st_type::mov, {{REGISTER}, {REGISTER, NAME, OFFSET, IMM}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]),
				{}
			};
		});
		recipe(st_type::mov, {{NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			// You can't mov from memory to other memory, so we
			// must clobber a register (r15X) with the value of the src
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(p[1]) + ", %" + reg{reg_name::r15, s[0]}.to_string() + \
				"\nmov" + S(s[0]) + " %" + reg{reg_name::r15, s[0]}.to_string() + ", " + N(p[0]),
				{reg_name::r15}
			};
		});

		recipe(st_type::addrof, {{REGISTER}, {NAME, OFFSET}}, [](auto &p, auto &s){
			// Take the N of p[1] and lea it into [0]
			return recipe::result_type{
				"leaq " + N(p[1]) + ", " + N(p[0]),
				{}
			};
		});
		recipe(st_type::addrof, {{NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			// leaq only works with registers, so we have to clobber r15
			return recipe::result_type {
				"leaq " + N(p[1]) + ", %r15\n" + 
				"mov %r15, " + N(p[0]),
				{reg_name::r15}
			};
		});

		// add
		
		recipe(st_type::add, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			if (p[0] != p[1]) throw std::runtime_error("not implemented trap: add p0 != p1");
			return recipe::result_type {
				"add" + S(s[0]) + " " + N(p[2]) + ", " + N(p[1]),
				{}
			};
		});
		recipe(st_type::add, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			if (p[0] != p[1]) throw std::runtime_error("not implemented trap: add p0 != p1");
			return recipe::result_type {
				"mov" + S(s[0]) + " " + N(p[2]) + ", " + N((reg{reg_name::r15, s[0]})) + "\n" +
				"add" + S(s[0]) + " " + N((reg{reg_name::r15, s[0]})) + ", " + N(p[1]),
				{reg_name::r15}
			};
		});
		
		// mul
		
		recipe(st_type::mul, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			if (p[0] != p[1]) throw std::runtime_error("not implemented trap: mul p0 != p1");
			return recipe::result_type {
				"imul" + S(s[0]) + " " + N(p[2]) + ", " + N(p[1]),
				{}
			};
		});
		recipe(st_type::mul, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			if (p[0] != p[1]) throw std::runtime_error("not implemented trap: mul p0 != p1");
			return recipe::result_type {
				"mov" + S(s[0]) + " " + N(p[2]) + ", " + N((reg{reg_name::r15, s[0]})) + "\n" +
				"imul" + S(s[0]) + " " + N((reg{reg_name::r15, s[0]})) + ", " + N(p[1]),
				{reg_name::r15}
			};
		});

		// div
		// is a piece of crap :)
		
		// in order to divide, the argument must be placed into a different register. also the
		// divisor must be half the size, so we use the lower half. this also clobbers the accumulator.
		// if the accumulator is also the divisor
		
		recipe(st_type::div, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET, IMM}}, [](auto &p, auto &s){
				if (p[0] != p[1]) throw std::runtime_error("not implemented trap: div p0 != p1");
				std::string text{};
				std::set<reg_name> clobber{reg_name::a, reg_name::d};
				param_type result = p[0];

				// first we need to check the size so we can move things correctly
				if ((std::holds_alternative<reg>(p[0]) && std::get<reg>(p[0]).name == reg_name::a) || (std::get<reg>(p[0]).name == reg_name::d && s[0] >= 2)) {
					// alright. the source will be clobbered, so move it into r14
					text += "mov" + S(s[0]) + " " + N(p[0]) + ", " + N((reg{reg_name::r14, s[0]})) + "\n";
					p[0] = reg{reg_name::r14, s[0]};
					clobber.insert(reg_name::r14);
				}
				if ((std::holds_alternative<reg>(p[2]) && std::get<reg>(p[2]).name == reg_name::a) || (std::get<reg>(p[2]).name == reg_name::d && s[2] >= 2)) {
					text += "mov" + S(s[0]) + " " + N(p[2]) + ", " + N((reg{reg_name::r13, s[2]})) + "\n";
					p[2] = reg{reg_name::r13, s[2]};
					clobber.insert(reg_name::r13);
				}

				// Now we need to move p[0] into the right location
				// if size == 1, we need to copy with sign extend into the next highest register
				if (s[0] == 1) {
					text += "movsbw " + N(p[0]) + ", %ax\n";
				}
				else {
					text += "mov " + N(p[0]) + ", " + N((reg{reg_name::d, s[0]})) + "\n";
					text += "mov " + N(p[0]) + ", " + N((reg{reg_name::a, s[0]})) + "\n";
				}

				// Now we need to check the imm
				if (std::holds_alternative<long>(p[2])) {
					if (s[1] == 1) ++s[1];
					text += "mov" + S(s[1]-1) + " " + N(p[2]) + ", " + N((reg{reg_name::r14, s[1]-1})) + "\n";
					p[2] = reg{reg_name::r14, s[1]-1};
				}
				else if (std::holds_alternative<reg>(p[2])) {
					if (s[1] == 1) ++s[1];
					std::get<reg>(p[2]).size = s[1]-1;
				}
				else if (s[0] != 1) {
					// We need to move the pointer by (1 >> (s-1)) units forwards.
					uint64_t offset = (1 << (s[0]-1)); // 2^s / 2
					if (std::holds_alternative<std::string>(p[2])) {
						std::get<std::string>(p[2]) += "+" + std::to_string(offset);
					}
					else {
						std::get<int16_t>(p[2]) += offset;
					}
				}

				// Alright. We are now finally ready to execute the idiv instruction. holy poo
				text += "idiv" + S((s[0] == 1 ? 1 : s[0]-1)) + " " + N(p[2]) + "\n";

				// But we're not done. Now we need to move the result
				if (s[0] == 1) {
					text += "movb %al, " + N(result);
				}
				else {
					text += "movs" + S(s[0]-1) + S(s[0]);
				        text += " " + N((reg{reg_name::a, (s[0] == 1 ? 1 : s[0]-1)})) + ", " + N((reg{reg_name::a, s[0]})) + "\n";
					text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(result);
				}
				if (std::holds_alternative<reg>(result) && std::get<reg>(result).name == reg_name::a) {
					clobber.erase(clobber.begin());
				}
				return recipe::result_type{text, clobber};
		});
		
		// neg (a simple one yay)
		
		recipe(st_type::neg, {{REGISTER, NAME, OFFSET}, {IMM}}, [](auto &p, auto &s){
			long result = -std::get<long>(p[1]);	
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(result) + ", " + N(p[1]),
				{}
			};
		});
		recipe(st_type::neg, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}}, [](auto &p, auto &s){
			if (p[0] != p[1]) throw std::runtime_error("not implemented: neg p0 != p1 && p1 is not imm");
			return recipe::result_type{
				"neg" + S(s[0]) + " " + N(p[0]),
				{}
			};
		});

		// ifnz is handled specially
		
		// read
		
		recipe(st_type::read, {{REGISTER}, {REGISTER}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[0]) + " (" + N(p[1]) + "), " + N(p[0]),
				{}
			};
		});
		recipe(st_type::read, {{REGISTER}, {OFFSET, NAME}}, [](auto &p, auto &s){
			return recipe::result_type{
				"movq " + N(p[1]) + ", " + N((reg{std::get<reg>(p[0]).name, 4})) + "\n" + 
				"mov" + S(s[0]) + " (" + N((reg{std::get<reg>(p[0]).name, 4})) + "), " + N(p[0]),
				{}
			};
		});
		recipe(st_type::read, {{OFFSET, NAME}, {REGISTER, OFFSET, NAME}}, [](auto &p, auto &s){
			return recipe::result_type{
				"movq " + N(p[1]) + ", %r15\n" +
				"mov" + S(s[0]) + " (%r15), " + N((reg{reg_name::r15, s[0]})) + "\n" +
				"mov" + S(s[0]) + " " + N((reg{reg_name::r15, s[0]})) + ", " + N(p[0]),
				{reg_name::r15}
			};
		});

		// write
		
		recipe(st_type::write, {{REGISTER}, {REGISTER, IMM}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[1]) + " " + N(p[1]) + ", (" + N(p[0]) + ")",
				{}
			};
		});
		recipe(st_type::write, {{REGISTER}, {OFFSET, NAME}}, [](auto &p, auto &s){
			reg clobber = (std::get<reg>(p[0]).name == reg_name::r15) ? reg{reg_name::r14, 4} : reg{reg_name::r15, 4};
			return recipe::result_type{
				"movq " + N(p[1]) + ", " + N(clobber) + "\n" +
				"mov" + S(s[1]) + " " + N(clobber) + ", (" + N(p[0]) + ")",
				{clobber.name}
			};
		});
		recipe(st_type::write, {{OFFSET, NAME}, {OFFSET, NAME, REGISTER, IMM}}, [](auto &p, auto &s){
			// Convert offset, name into a register.
			reg clobber{reg_name::r15, 4};
			if (std::holds_alternative<reg>(p[1]) && std::get<reg>(p[1]).name == reg_name::r15) {
				clobber.name = reg_name::r14;
			}
			std::string text = "movq " + N(p[0]) + ", " + N(clobber) + "\n";
			if (std::holds_alternative<reg>(p[1]) || std::holds_alternative<long>(p[1])) {
				return recipe::result_type{
					text + "mov" + S(s[1]) + " " + N(p[1]) + ", (" + N(clobber) + ")",
					{clobber.name}
				};
			}
			else {
				reg other_clobber{reg_name::r13, s[1]};
				return recipe::result_type{
					text + 
					"movq " + N(p[1]) + ", " + N(other_clobber) + "\n" +
					"mov" + S(s[1]) + " " + N(other_clobber) + ", (" + N(clobber) + ")",
					{clobber.name, other_clobber.name}
				};
			}
		});

		// eq & gt:
		//
		// compare a & b (optionally loading b into a register). use lahf to grab the flags and put the zero or c flags
		// into the target. shift the target
		
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {IMM}, {IMM}}, [](auto &p, auto &s){
			long result = static_cast<long>(std::get<long>(p[1]) == std::get<long>(p[2]));
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(result) + ", " + N(p[0]),
				{}
			};
		});
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {REGISTER, IMM}, {REGISTER, NAME, OFFSET}}, [](auto &p, auto &s){
			std::string text;
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "xor %rax, %rax\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "and $0x4000, %ax\n";
			text += "shr $14, %ax\n";
			text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(p[0]);
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			std::string text;
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "xor %rax, %rax\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "and $0x4000, %ax\n";
			text += "shr $14, %ax\n";
			text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(p[0]);
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			std::string text;
			std::swap(p[1], p[2]);
			reg temp{reg_name::a, s[2]};
			text += "mov" + S(s[2]) + " " + N(p[2]) + ", " + N(temp) + "\n";
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(temp) + "\n";
			text += "xor %rax, %rax\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "and $0x4000, %ax\n";
			text += "shr $14, %ax\n";
			text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(p[0]);
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});

		// gt
		
		recipe(st_type::gt, {{REGISTER, NAME, OFFSET}, {IMM}, {IMM}}, [](auto &p, auto &s){
			long result = static_cast<long>(std::get<long>(p[1]) > std::get<long>(p[2]));
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(result) + ", " + N(p[0]),
				{}
			};
		});
		recipe(st_type::gt, {{REGISTER, NAME, OFFSET}, {REGISTER, IMM}, {REGISTER, NAME, OFFSET}}, [](auto &p, auto &s) {
			static int label_count = 0;
			int label = label_count++;
			std::string text;
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "mov" + S(s[0]) + " $1, " + N(p[0]) + "\n";
			text += "jg @ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += "@ggl" + std::to_string(label) + ":\n";
			return recipe::result_type{
				text,
				{}
			};
		});
		recipe(st_type::gt, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			static int label_count = 0;
			int label = label_count++;
			std::string text;
			std::set<reg_name> clobber{};
			if (std::holds_alternative<long>(p[2])) {
				reg temp{reg_name::a, s[2]};
				text += "mov" + S(s[2]) + " " + N(p[2]) + ", " + N(temp) + "\n";
				p[2] = temp;
				clobber.insert(reg_name::a);
			}
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "mov" + S(s[0]) + " $1, " + N(p[0]) + "\n";
			text += "jg @ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += "@ggl" + std::to_string(label) + ":\n";
			return recipe::result_type{
				text,
				clobber
			};
		});
		recipe(st_type::gt, {{REGISTER, NAME, OFFSET}, {NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			static int label_count = 0;
			int label = label_count++;
			std::string text;
			reg temp{reg_name::a, s[2]};
			text += "mov" + S(s[2]) + " " + N(p[2]) + ", " + N(temp) + "\n";
			text += "cmp" + S(s[0]) + " " + N(p[1]) + ", " + N(temp) + "\n";
			text += "mov" + S(s[0]) + " $1, " + N(p[0]) + "\n";
			text += "jg @ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += "@ggl" + std::to_string(label) + ":\n";
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});

		// cast
		//
		// an even bigger oh poo
		
		recipe(st_type::cast, {{REGISTER, NAME, OFFSET}, {IMM}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[2]),
				{}
			};
		});
		recipe(st_type::cast, {{REGISTER, NAME, OFFSET}, {REGISTER}}, [](auto &p, auto &s){
			// Check the sizes of the operands. If s0 > s1, this is an upcast.
			// First though, make sure they aren't equal. If they are, just do a mov
			if (s[0] == s[1] || s[0] < s[1]){
				return recipe::result_type{
					"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
					{}
				};
			}
			else {
				// Upcast. Use movsx/movsxd
				return recipe::result_type{
					"movs" + S(s[1]) + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
					{}
				};
			}
		});
		recipe(st_type::cast, {{REGISTER, NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			if (s[0] == s[1]) {
				if (std::holds_alternative<reg>(p[0])) {
					return recipe::result_type{
						"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
						{}
					};
				}
				else {
					reg clobber{reg_name::r15, s[0]};
					return recipe::result_type{
						"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(clobber) + "\n" +
						"mov" + S(s[0]) + " " + N(clobber) + ", " + N(p[0]),
						{reg_name::r15}
					};
				}
			}
			else if (s[0] > s[1]) {
				if (std::holds_alternative<reg>(p[0])) {
					return recipe::result_type{
						"movs" + S(s[1]) + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
						{}
					};
				}
				else {
					reg clobber{reg_name::r15, s[0]};
					return recipe::result_type{
						"movs" + S(s[1]) + S(s[0]) + " " + N(p[1]) + ", " + N(clobber) + "\n" +
						"mov" + S(s[0]) + " " + N(clobber) + ", " + N(p[0]),
						{reg_name::r15}
					};
				}
			}
			else {
				if (std::holds_alternative<reg>(p[0])) {
					std::string text{};
					reg conj_{std::get<reg>(p[0]).name, s[1]};
					text += "mov" + S(s[1]) + " " + N(p[1]) + ", " + N(conj_) + "\n";
					// zero out the upper portion
					uint64_t bmask = ~0;
					bmask <<= s[0]*8;
					bmask = ~bmask;
					text += "and" + S(s[1]) + " $" + std::to_string(bmask) + ", " + N(conj_);
					return recipe::result_type{text, {}};
				}
				else {
					std::string text{};
					reg conj_{reg_name::r15, s[1]};
					text += "mov" + S(s[1]) + " " + N(p[1]) + ", " + N(conj_) + "\n";
					// zero out the upper portion
					uint64_t bmask = ~0;
					bmask <<= s[0]*8;
					bmask = ~bmask;
					text += "and" + S(s[1]) + " $" + std::to_string(bmask) + ", " + N(conj_) + "\n";
					text += "mov" + S(s[0]) + " " + N((reg{reg_name::r15, s[0]})) + ", " + N(p[0]);
					return recipe::result_type{text, {reg_name::r15}};
				}
			}
		});


                #undef ANY
		#undef REGISTER
		#undef NAME
		#undef OFFSET
		#undef IMM
		#undef S
		#undef N
		#undef RECIPE
	}

	void codegenerator::prepare() {
		for (auto &[_, c] : func_compileunits) prepare_unit(&c);
		prepare_unit(&global_cu);
	}

	void codegenerator::prepare_unit(compilation_unit *u) {
		// Prepare a compile unit for compiling.
		//
		// This should ensure that the three address code gets converted to two address code (twac).
		// It does this by checking for all instructions (excluding a few) that have three operands if the first
		// and second parameters are not equal. If so, it converts it into something similar to
		//
		// add c, a, b
		//
		// to
		//
		// mov c, a
		// add c, c, b
		// This is a guarantee, so the code generator only has to handle this case
		// and not a billion other cases.
		// Although this can be a pessimization in some small corner cases (like avoiding use of the three operand form
		// of imul or lea), it simplifies code generation a whole lot, so I've decided to use this
		//
		// In the future, I may change this function to run after RA so it can take advantage of whether
		// or not it actually needs to do this preparation. This would require additional generator logic as well, so I've decided
		// against it for the time being.
		
		traverse_f(u->start, [&](statement * &s){
			// Operate on s, converting it into the mov and adding a new statement afterwords.
			
			switch (s->t) {
			case st_type::add:
			case st_type::mul:
			case st_type::div:
				{
					// Check if p.lhs() == p[1]
					if (s->lhs() == s->params[1]) return; // ok, all is good
					// Otherwise, we need to convert to a twac instruction
					addr_ref rhs  = s->rhs();
					st_type  orig = s->t;

					s->reinit(st_type::mov, s->lhs(), s->params[1]);
					auto st = std::make_unique<statement>(orig, addr_ref(s->lhs()), addr_ref(s->lhs()), rhs);
					st->next = s->next;
					s->next = st.get();
					all_statements.emplace_back(std::move(st));
					break;
				}
			// comparison operators (due to my crappy codegen) don't actually need tac -> twac.
			// addrof == lea
			// fcall is a special snowflake
			case st_type::neg:
				{
					if (s->lhs() == s->rhs()) return;
					if (ai_num(s->rhs())) return; // allow r0 = -1 to go through

					// Convert to mov lhs(), rhs(), neg lhs() lhs()
					s->reinit(st_type::mov, s->lhs(), s->rhs());
					auto st = std::make_unique<statement>(st_type::neg, s->lhs(), s->lhs());
					st->next = s->next;
					s->next = st.get();
					all_statements.emplace_back(std::move(st));
					break;
				}
			default:
				break;
			}
		});
	}

        recipe::result_type codegenerator::pick(st_type t, std::vector<param_type> params, std::vector<int> sizes) {
		std::vector<recipe::result_type> results{};
		std::exception last_ex;
		for (auto &e : recipes[t]) {
			if (e.is_valid(params)) {
				try {
					auto c1 = std::vector<param_type>(params);
					auto c2 = std::vector<int>(sizes);
					results.push_back(e.func(c1, c2));
				}
				catch (std::exception &e) {
					last_ex = e;
				}
			}
		}
		if (results.empty()) throw last_ex;

		if (results.size() == 1) return results.front();
		// Sort the results, first by number of clobbers, next by length
		std::sort(results.begin(), results.end(), recipe::compare_type());
		return results.front();
	}
}
