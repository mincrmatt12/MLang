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
				"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
				{}
			};
		});
		recipe(st_type::mov, {{REGISTER}, {REGISTER, NAME, OFFSET, IMM}}, [](auto &p, auto &s){
			return recipe::result_type{
				"mov" + S(s[0]) + " " + N(p[1]) + ", " + N(p[0]),
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
			text += "xor %rax, %rax\n";
			text += "cmp" + S(s[2]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "andb $0b01000000, %ah\n";
			text += "shr $14, %ax\n";
			text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(p[0]);
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			std::swap(p[1], p[2]);
			std::string text;
			text += "xor %rax, %rax\n";
			text += "cmp" + S(s[2]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "andb $0b01000000, %ah\n";
			text += "shr $14, %ax\n";
			text += "mov" + S(s[0]) + " " + N((reg{reg_name::a, s[0]})) + ", " + N(p[0]);
			return recipe::result_type{
				text,
				{reg_name::a}
			};
		});
		recipe(st_type::eq, {{REGISTER, NAME, OFFSET}, {NAME, OFFSET}, {NAME, OFFSET}}, [](auto &p, auto &s){
			std::string text;
			reg temp{reg_name::a, s[2]};
			text += "mov" + S(s[2]) + " " + N(p[2]) + ", " + N(temp) + "\n";
			text += "xor %rax, %rax\n";
			text += "cmp" + S(s[1]) + " " + N(p[1]) + ", " + N(temp) + "\n";
			text += "lahf\n";
			if (std::holds_alternative<reg>(p[0])) {
				if (std::get<reg>(p[0]).name != reg_name::a) {
					text += "xor " + N(p[0]) + ", " + N(p[0]) + "\n";	
				}
			}
			else {
				text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			}
			text += "andb $0b01000000, %ah\n";
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
			text += "cmp" + S(s[2]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "mov" + S(s[0]) + " $1, " + N(p[0]) + "\n";
			text += "jg .ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += ".ggl" + std::to_string(label) + ":";
			return recipe::result_type{
				text,
				{}
			};
		});
		recipe(st_type::gt, {{REGISTER, NAME, OFFSET}, {REGISTER, NAME, OFFSET}, {REGISTER, IMM}}, [](auto &p, auto &s){
			std::swap(p[1], p[2]);
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
			text += "cmp" + S(s[2]) + " " + N(p[1]) + ", " + N(p[2]) + "\n";
			text += "mov" + S(s[0]) + " $1, " + N(p[0]) + "\n";
			text += "jg .ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += ".ggl" + std::to_string(label) + ":";
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
			text += "jg .ggl" + std::to_string(label) + "\n";
			text += "mov" + S(s[0]) + " $0, " + N(p[0]) + "\n";
			text += ".ggl" + std::to_string(label) + ":";
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
					"mov" + S(s[0]) + " " + N((reg{std::get<reg>(p[1]).name, s[0]})) + ", " + N(p[0]),
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
						"mov" + S(s[0]) + " " + N((reg{std::get<reg>(p[1]).name, s[0]})) + ", " + N(p[0]),
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

	static int bytes_to_s(int b) {
		switch (b) {
			case 64:
				return 4;
			case 32:
				return 3;
			case 16:
				return 2;
			case 8:
				return 1;
			default:
				return 0;
		}
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

					if (rhs.rt != s->params[1].rt) {
						if (rhs.rt == s->lhs().rt) {
							rhs = s->params[1];
							s->reinit(st_type::cast, s->lhs(), s->rhs());
						}
						else {
							s->reinit(st_type::cast, s->lhs(), s->params[1]);
						}
					}
					else s->reinit(st_type::mov, s->lhs(), s->params[1]);
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
			// Fix movs of different types
			case st_type::mov:
				{
					if (s->lhs().rt != s->rhs().rt) {
						s->t = st_type::cast;
						break;
					}
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

	void codegenerator::generate() {
		prepare();
		// Generate all functions. At some point, we should have built main. When building main, we also build the global initscope before anything else
		for (auto &[name, unit] : func_compileunits) {
			generate_unit(name, unit);
		}

		// Now generate the bss segment. Find all globals.
		std::set<std::pair<std::string, int>> globals{};
		for (auto &[_, unit] : func_compileunits) {
			traverse_f(unit.start, [&](auto &s){
				auto find_global = [&](addr_ref &reg) {
					if (ai_ident(reg) && reg.ident.type == id_type::global_var) {
						globals.insert({reg.ident.name, reg.rt.ptr == nullptr ? reg.rt.size : 64});
					}
				};

				s->for_all_read(find_global);
				s->for_all_write(find_global);
			});
		}
		traverse_f(global_cu.start, [&](auto &s){
			auto find_global = [&](addr_ref &reg) {
				if (ai_ident(reg) && reg.ident.type == id_type::global_var) {
					globals.insert({reg.ident.name, reg.rt.ptr == nullptr ? reg.rt.size : 64});
				}
			};

			s->for_all_read(find_global);
			s->for_all_write(find_global);
		});

		// Allocate all globals.
		for (auto &[name, size] : globals) {
			bss_section += name + ": ";

			switch (size) {
				case 8:
					bss_section += ".byte\n";
					break;
				case 16:
					bss_section += ".word\n";
					break;
				case 32:
					bss_section += ".value\n";
					break;
				default:
					bss_section += ".quad\n";
					break;
			}
		}

	}

	void codegenerator::generate_unit(std::string label_name, compilation_unit &c) {
		// Output label_name to text section
		text_section += label_name + ":\n";
		// Check if global
		if (label_name == "main" && global_cu.start != nullptr) {
			text_section += "pushq %rdi\npushq %rsi\n";
			generate_unit("$GLOBAL", global_cu);
			text_section += "popq %rsi\npopq %rdi\n";
		}
		auto [allocations, szs] = allocate_registers(c);

		// We need to preserve various registers, so check if any of these are used
		std::set<reg_name> preserved;
		int stack_amount = 0;
		for (auto &e : allocations) {
			if (std::holds_alternative<reg>(e)) {
				reg_name r = std::get<reg>(e).name;
				if (r == reg_name::b ||
				    r >= reg_name::r12) {
					preserved.insert(r);
				}
			}
			else if (std::holds_alternative<int16_t>(e)) {
				int16_t i = -std::get<int16_t>(e);
				stack_amount = std::max(stack_amount, (int)i);
			}
		}
		// Generate the base pointer & preserved registers
		//
		// pushq %rbp
		// mov %rsp, %rbp
		// sub $sa, %rsp
		// pushq <preserved>
		
		text_section += "pushq %rbp\nmov %rsp, %rbp\nsub $" + std::to_string(stack_amount) + ", %rsp\n";
		for (auto &e : preserved) {
			text_section += "pushq " + nameoffset_to_str(reg{e, 4}) + "\n";
		}

		// Now we need to move parameters into the right place
		std::vector<reg_name> parameter_locations{
			reg_name::di,
			reg_name::si,
			reg_name::d,
			reg_name::c,
			reg_name::r8,
			reg_name::r9
		};

		int paramoffset = 0;
		if (c.num_params > 6) {
			paramoffset = -8 * (c.num_params - 6);
		}

		for (int i = 0; i < c.num_params; i++) {
			// See if there are still things on the registers.
			if (i > szs.size()) break; // Parameter not used, ignore the rest
			if (parameter_locations.empty()) {
				// Darn. We have to move this register into the allocated space for the parameter.
				
				text_section += "mov" + ssuffix(szs[i]) + " " + std::to_string(paramoffset) + "(%rbp), " + nameoffset_to_str(reg{reg_name::r15, szs[i]}) + "\n";
				text_section += "mov" + ssuffix(szs[i]) + " " + nameoffset_to_str(reg{reg_name::r15, szs[i]}) + ", " + nameoffset_to_str(allocations[i]) + "\n";
				paramoffset += 8;
			}
			else {
				// Yay! We have a register. Maybe. Possibly
				reg_name source = parameter_locations.front();
				parameter_locations.erase(parameter_locations.begin());

				if (std::holds_alternative<reg>(allocations[i]) && std::get<reg>(allocations[i]).name == source) continue; // Already in correct location.

				text_section += "mov" + ssuffix(szs[i]) + " " + nameoffset_to_str(reg{source, szs[i]}) + ", " + nameoffset_to_str(allocations[i]) + "\n";
			}
		}

		// Ok, the stack has now been setup. We can begin processing. Just kidding. First we have to get labels
		
		auto indices = std::map<statement *, int>{}; // Get the canonical order of all of these things.
		auto labels  = std::map<int, int>{};
		auto trav    = std::map<int, statement *>{};

		traverse_f(c.start, [&](statement *k, int v){
			indices[k] = v;
			trav[v] = k;
		});

		auto add_label = [&, l=0](int i) mutable {
			if (labels.count(i) == 0) {
				labels[i] = l++;
			}	
		};


		// Assume nothing requires a label. We only need one if we decide we need to jump there at some point.
		// We only jump somewhere if the index of the next pointer is not equal to the index of the current
		// statement + 1.
		// Since conditional statements ALWAYS jump, we can assume they need labels whenever we see them.
		
		// Step one: determine what needs labels by iterating over everything in the indices map
		for (auto &[index, st] : trav) {
			if (st->cond != nullptr) add_label(indices[st->cond]);
			// Check if the index of the next pointer (if it exists) is equal to 1+index
			if (st->next != nullptr) {
				if (indices[st->next] != index+1) add_label(indices[st->next]);
			}
		}

		// Ok, now we have all of the labels ready to go.
		for (auto &[i, st] : trav) {
			// Check if we need to insert a label
			if (labels.count(i) > 0) {
				text_section += ".LC" + label_name + std::to_string(labels[i]) + ":\n";
			}

			// Assemble the parameter types
			std::vector<param_type> types{};
			std::vector<int> 	sizes{};

			for (auto &p : st->params) {
				if (ai_reg(p)) {
					int s = bytes_to_s(p.rt.ptr == nullptr ? p.rt.size : 64);
					
					// Ok, types = reg{alloc[p], s}
					auto alloc = allocations[p.num];
					if (std::holds_alternative<reg>(alloc)) {
						types.push_back(reg{std::get<reg>(alloc).name, s});
						sizes.push_back(s);
					}
					else {
						int s2 = szs[p.num];
						int offset = 1 << (s2 - s);
						int16_t sp = -std::get<int16_t>(alloc);
						types.push_back(static_cast<int16_t>(sp + static_cast<int16_t>(offset)));
						sizes.push_back(s);
					}
				}
				else if (ai_num(p)) {
					int s = bytes_to_s(p.rt.ptr == nullptr ? p.rt.size : 64);
					sizes.push_back(s);
					types.push_back(p.num);
				}
				else if (ai_ident(p)) {
					int s = bytes_to_s(p.rt.ptr == nullptr ? p.rt.size : 64);
					types.push_back(p.ident.name);
					sizes.push_back(s);
				}
			}

			switch (st->t) {
				case st_type::fcall:
					{
						// fcall. Clobbers a _whole_ lot of stuff.
						// Essentially we need to check quite a few things:
						//   - push clobberable registers (which are any register used that are not in the preserve list)
						//   - move arguments into the correct registers, pushing parameters if required.
						//   - call the function
						//   - move the result
						//   - un clobber, being careful not to overwrite the result
						
						std::set<reg_name> clobbering{};
						for (auto i : allocations) {
							if (std::holds_alternative<reg>(i) && i != types[0]) {
								if (!preserved.count(std::get<reg>(i).name)) {
									// We need to clobber
									clobbering.insert(std::get<reg>(i).name);
								}
							}
						}

						// Push all clobberable registers
						for (auto &e : clobbering) {
							text_section += "pushq " + nameoffset_to_str(reg{e, 4}) + "\n";
						}
						// Move arguments into correct locations.
						std::vector<reg_name> parameter_locations{
							reg_name::di,
							reg_name::si,
							reg_name::d,
							reg_name::c,
							reg_name::r8,
							reg_name::r9
						};

						// If varargs, set al to 0.
						if (ai_ident(st->params[1]) && st->params[1].ident.type == id_type::extern_function && ext_functions[st->params[1].ident.index].varargs) {
							text_section += "movq $0, %rax\n";
						}

						for (int i = 0; i < types.size()-2; i++) {
							// Move the parameter into the next available location, or push onto the stack
							if (parameter_locations.empty()) {
								// If the argument is a stack parameter, load it into r15 first then push r15
								if (!std::holds_alternative<reg>(types[i+2]) && !std::holds_alternative<long>(types[i+2])) {
									text_section += "xor %r15, %r15\n";
									text_section += "mov" + ssuffix(sizes[i+2]) + " " + nameoffset_to_str(types[i+2]) + ", " + nameoffset_to_str(reg{reg_name::r15, sizes[i+2]}) + "\n";
									text_section += "pushq %r15\n";
								}
								else if (std::holds_alternative<long>(types[i+2])) {
									text_section += "pushq " + nameoffset_to_str(types[i+2]) + "\n";
								}
								else {
									// Make sure we haven't already pushed the parameters.
									if (clobbering.count(std::get<reg>(types[i+2]).name)) {
										// HANDLE THIS MATTHEW YOU LAZY PIECE OF CRAP
										throw std::runtime_error("Invalid register allocation detected for fcall with >6");
									}
									else {
										text_section += "pushq " + nameoffset_to_str(reg{std::get<reg>(types[i+2]).name, 4});
									}
								}
							}
							else {
								reg_name target = parameter_locations.front();
								parameter_locations.erase(parameter_locations.begin());

								if (std::holds_alternative<reg>(types[i+2]) && clobbering.count(std::get<reg>(types[i+2]).name)) {
									// Work out where on the stack it is.
									int offset = 0;
									for (auto j = clobbering.rbegin(); *j != std::get<reg>(types[i+2]).name; ++j) {
										offset += 8;
									}
									text_section += "xor " + nameoffset_to_str(reg{target, 4}) + ", " + nameoffset_to_str(reg{target, 4}) + "\n";
									text_section += "mov" + ssuffix(sizes[i+2]) + " " + std::to_string(offset) + "(%rsp), " + nameoffset_to_str(reg{target, sizes[i+2]}) + "\n";
								}
								else {
									text_section += "xor " + nameoffset_to_str(reg{target, 4}) + ", " + nameoffset_to_str(reg{target, 4}) + "\n";
									text_section += "mov" + ssuffix(sizes[i+2]) + " " + nameoffset_to_str(types[i+2]) + ", " + nameoffset_to_str(reg{target, sizes[i+2]}) + "\n";
								}
							}
						}

						// Call the function
						text_section += "call " + nameoffset_to_str(types[1]) + "\n";
						if (!(std::holds_alternative<reg>(types[0]) && std::get<reg>(types[0]).name == reg_name::a)) {
							// Move a into the target
							text_section += "mov" + ssuffix(sizes[0]) + " " + nameoffset_to_str(reg{reg_name::a, sizes[0]}) + ", " + nameoffset_to_str(types[0]) + "\n";
						}

						// Repair the clobbering.
						for (auto i = clobbering.rbegin(); i != clobbering.rend(); ++i) {
							text_section += "popq " + nameoffset_to_str(reg{*i, 4}) + "\n";
						}
					}
					break;
				case st_type::ret:
					{
						// Take the result into a
						
						text_section += "mov" + ssuffix(sizes[0]) + " " + nameoffset_to_str(types[0]) + ", " + nameoffset_to_str(reg{reg_name::a, sizes[0]}) + "\n";

						// Pop everything.
						for (auto i = preserved.rbegin(); i != preserved.rend(); i++) {
							text_section += "pushq " + nameoffset_to_str(reg{*i, 4}) + "\n";
						}
						// Add the stack.
						text_section += "addq $" + std::to_string(stack_amount) + ", %rsp\n" +
						/* Now restore the bp */
								"popq %rbp\n" +
						/* Now add ret */
								"ret\n";
					
					}
					break;
				case st_type::ifnz:
					{
						text_section += "cmp" + ssuffix(sizes[0]) + " $0, " + nameoffset_to_str(types[0]) + "\n" +
							        "jne .LC" + label_name + std::to_string(labels[indices[st->cond]]) + "\n";
						break;
					}
				default:
					{
						// Pick the best template
						auto result = pick(st->t, types, sizes);
						// Check the clobber info.
						// Remove parameters that are results
						st->for_all_write([&](auto &reg){
							if (ai_reg(reg)) {
								if (std::holds_alternative<x86_64::reg>(types[0])) {
									if (auto i = std::find(result.second.begin(), result.second.end(), std::get<x86_64::reg>(types[0]).name); i != result.second.end()) {
										result.second.erase(i);
									}	
								}
							}
						});
						for (auto &c : result.second) {
							text_section += "pushq " + nameoffset_to_str(reg{c, 4}) + "\n";
						}
						// Put the text onto text
						text_section += result.first + "\n";
						// Restore clobbered registers.
						for (auto c = result.second.rbegin(); c != result.second.rend(); c++) {
							text_section += "popq " + nameoffset_to_str(reg{*c, 4}) + "\n";
						}
					}

			}

			// Check if the next statement has the next index
			if (indices[st->next] != i + 1 && st->next != nullptr) {
				// Add a jump
				text_section += "jmp .LC" + label_name + std::to_string(labels[indices[st->next]]) + "\n";
			}
		}

		// Pop everything.
		for (auto i = preserved.rbegin(); i != preserved.rend(); i++) {
			text_section += "pushq " + nameoffset_to_str(reg{*i, 4}) + "\n";
		}
		// Add the stack.
		text_section += "addq $" + std::to_string(stack_amount) + ", %rsp\n" +
		/* Now restore the bp */
			        "popq %rbp\n";
	}

	// Register "allocator"
	std::pair<std::vector<param_type>, std::vector<int>> codegenerator::allocate_registers(compilation_unit &u) {
		std::vector<reg_name> remaining_registers{
			reg_name::di,
			reg_name::si,
			reg_name::d,
			reg_name::c,
			reg_name::r8,
			reg_name::r9,
			reg_name::a,
			reg_name::b,
			reg_name::r10,
			reg_name::r11,
			reg_name::r12,
			reg_name::r13,
			reg_name::r14,
			reg_name::r15
		};

		std::map<int, bool> reg_usages{};
		std::map<int, int>  highest_size{};
		
		// First, collect all usages of registers. This will make sure that things like addrof work properly
		traverse_f(u.start, [&](statement *s){
			switch (s->t) {
				case st_type::addrof:
					{
						if (ai_reg(s->rhs())) {
							reg_usages[s->rhs().num] = true;
						}
						[[fallthrough]];
					}
				default:
					{
						auto f = [&](addr_ref &reg) {
							if (ai_reg(reg) && reg_usages.count(reg.num) == 0) {
								reg_usages[reg.num] = false;
							}
							if (ai_reg(reg)) {
								int size = reg.rt.ptr == nullptr ? reg.rt.size : 64;
								highest_size[reg.num] = std::max(highest_size[reg.num], size);
							}
							
						};
						s->for_all_read(f);
						s->for_all_write(f);
						break;
					}
			}
		});

		int sp = 0;
		std::vector<param_type> allocations;
		std::vector<int>        szs;

		for (auto &[reg_num, force_stack] : reg_usages) {
			if (force_stack || remaining_registers.empty()) {
				// Allocate off of the stack. Allocate the largest known number of bytes, and let the actual code determine where to start
				// the pointer based on the size used.
				sp -= highest_size[reg_num]; 
				allocations.push_back(static_cast<int16_t>(sp));
				int s = bytes_to_s(highest_size[reg_num]);
				szs.push_back(s);
			}
			else {
				reg_name r = remaining_registers.front();
				remaining_registers.erase(remaining_registers.begin());
				int s = bytes_to_s(highest_size[reg_num]);
				allocations.push_back(reg{r, s});
				szs.push_back(s);
			}
		}
		return {allocations, szs};
	}
}
