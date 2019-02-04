#include "codegen.h"

namespace x86_64 {
	const char * registers[4][14] = {
		{"dil" , "sil" , "dl"  ,  "cl"  , "r8b" , "r9b" , "al"  , "bl"  , "r10b" , "r11b" , "r12b" , "r13b" , "r14b" , "r15b"} ,
		{"di"  , "si"  , "dx"  ,  "cx"  , "r8w" , "r9w" , "ax"  , "bx"  , "r10w" , "r11w" , "r12w" , "r13w" , "r14w" , "r15w"} ,
		{"edi" , "esi" , "edx" ,  "ecx" , "r8d" , "r9d" , "eax" , "ebx" , "r10d" , "r11d" , "r12d" , "r13d" , "r14d" , "r15d"} ,
		{"rdi" , "rsi" , "rdx" ,  "rcx" , "r8"  , "r9"  , "rax" , "rbx" , "r10"  , "r11"  , "r12"  , "r13"  , "r14"  , "r15"}
	};

	const char * sizes[4] = {
		"byte",
		"word",
		"dword",
		"qword"
	};

#define AnyS      {p_size::BYTE, p_size::WORD, p_size::DWORD, p_size::QWORD}
#define WordS     {p_size::WORD, p_size::DWORD, p_size::QWORD}
#define DWordS    {p_size::DWORD, p_size::QWORD}
#define SameAs(x) {{match_t::SAMEAS}, {}, x}
#define Reg(x, s) {{match_t::REG}, s, x}
#define AnyReg(s) {{match_t::REG}, s, ~0}
#define RegMem(s) {{match_t::REG, match_t::MEM}, s, ~0}
#define Any(s)	  {{match_t::REG, match_t::MEM, match_t::IMM}, s, ~0}
#define RegImm    {{match_t::REG, match_t::IMM}, AnyS, ~0}
#define Imm       {{match_t::IMM}, AnyS, ~0}
#define Mem(s)    {{match_t::MEM}, s, ~0}
#define Const(x)  {{match_t::CONSTIMM}, AnyS, x}
#define Ident     {{match_t::IDENT}, AnyS, 0}

	const recipe recipes[] = {
		/* NOP */
		{10, st_type::nop, "nop", {}},
		/* MOV */
		{10, st_type::mov,  "mov %0, %1", {AnyReg(AnyS), RegImm}},
		{11, st_type::mov,  "mov %0, %1", {Mem(AnyS), RegImm}},
		/* ADD */
		{ 9, st_type::add,  "inc %0", {RegMem(AnyS), SameAs(0), Const(1)}},
		{ 9, st_type::add,  "dec %0", {RegMem(AnyS), SameAs(0), Const(-1)}},
		{10, st_type::add,  "add %0, %2", {AnyReg(AnyS), SameAs(0), RegMem(AnyS)}},
		{11, st_type::add,  "add %0, %2", {Mem(AnyS), SameAs(0), AnyReg(AnyS)}},
		{10, st_type::add,  "add %0, %2", {RegMem(AnyS), SameAs(0), Imm}},
		{ 8, st_type::add,  "lea %0, [%1 + %2]", {AnyReg(AnyS), AnyReg(AnyS), RegImm}},
		/* MUL */
		{10, st_type::mul,  "imul %0, %2", {AnyReg(AnyS), SameAs(0), RegMem(AnyS)}},
		{11, st_type::mul,  "imul %0, %1, %2", {AnyReg(AnyS), RegMem(AnyS), Imm}},
		/* DIV */
		{ 9, st_type::div,  "idiv %0 | xor ah, ah", {Reg(6, {p_size::BYTE}), SameAs(0), RegMem({p_size::BYTE})}},
		{10, st_type::div,  "cwd | idiv %2", {Reg(6, {p_size::WORD}), SameAs(0), RegMem({p_size::WORD})}, {2}},
		{10, st_type::div,  "cdq | idiv %2", {Reg(6, {p_size::DWORD}), SameAs(0), RegMem({p_size::DWORD})}, {2}},
		{10, st_type::div,  "cqo | idiv %2", {Reg(6, {p_size::QWORD}), SameAs(0), RegMem({p_size::QWORD})}, {2}},
        /* MOD */
		{ 9, st_type::div,  "idiv %0 | shr ax, 8", {Reg(6, {p_size::BYTE}), SameAs(0), RegMem({p_size::BYTE})}},
		{10, st_type::div,  "cwd | idiv %2", {Reg(6, {p_size::WORD}), Reg(2, {p_size::WORD}), RegMem({p_size::WORD})}},
		{10, st_type::div,  "cdq | idiv %2", {Reg(6, {p_size::DWORD}), Reg(2, {p_size::DWORD}), RegMem({p_size::DWORD})}},
		{10, st_type::div,  "cqo | idiv %2", {Reg(6, {p_size::QWORD}), Reg(2, {p_size::QWORD}), RegMem({p_size::QWORD})}},
		/* NEG */
		{10, st_type::neg,  "neg %0", {RegMem(AnyS), SameAs(0)}},
		{11, st_type::neg,  "imul %0, %1, -1", {AnyReg(AnyS), RegMem(AnyS)}},
		/* IFNZ */
		{10, st_type::ifnz, "cmp %0, 0 | jne %l", {RegMem(AnyS)}},
		/* IFEQ */
		{10, st_type::ifeq, "cmp %0, %1 | je %l", {RegMem(AnyS), RegImm}},
		/* IFGT */
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {RegMem(AnyS), RegImm}},
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {AnyReg(AnyS), RegMem(AnyS)}},
		/* READ */
		{10, st_type::read, "movzx %q0, byte [%1]", {AnyReg({p_size::BYTE}), AnyReg({p_size::BYTE})}},
		{10, st_type::read, "movzx %q0, word [%1]", {AnyReg({p_size::WORD}), AnyReg({p_size::WORD})}},
		{11, st_type::read, "xor %q0, %q0 | mov %0, dword [%1]", {AnyReg({p_size::DWORD}), AnyReg({p_size::DWORD})}},
		{11, st_type::read, "xor %q0, %q0 | mov %0, qword [%1]", {AnyReg({p_size::QWORD}), AnyReg({p_size::QWORD})}},
		/* WRITE */
		{10, st_type::write, "mov byte [%0], %1", {RegImm, AnyReg({p_size::BYTE})}},
		{10, st_type::write, "mov word [%0], %1", {RegImm, AnyReg({p_size::WORD})}},
		{10, st_type::write, "mov dword [%0], %1", {RegImm, AnyReg({p_size::DWORD})}},
		{10, st_type::write, "mov qword [%0], %1", {RegImm, AnyReg({p_size::QWORD})}},
		/* EQ */
		{10, st_type::eq,   "cmp %1, %2 | sete %0 ", {RegMem(AnyS), RegMem(AnyS), RegImm}},
		/* GT */
		{10, st_type::gt,   "cmp %1, %2 | seta %0 ", {RegMem(AnyS), RegMem(AnyS), RegImm}},
		{10, st_type::gt,   "cmp %1, %2 | seta %0 ", {RegMem(AnyS), AnyReg(AnyS), RegMem(AnyS)}},
		/* CAST */
		{10, st_type::cast, "movsx %0, %1", {AnyReg(WordS), RegMem({p_size::BYTE})}},
		{10, st_type::cast, "movsx %0, %1", {AnyReg(DWordS), RegMem({p_size::WORD})}},
		{10, st_type::cast, "movsx %0, %1", {AnyReg({p_size::QWORD}), RegMem({p_size::DWORD})}},
		{11, st_type::cast, "xor %q0, %q0 | mov %0, %b1", {AnyReg({p_size::BYTE}), RegMem(WordS)}},
		{11, st_type::cast, "xor %q0, %q0 | mov %0, %w1", {AnyReg({p_size::WORD}), RegMem(DWordS)}},
		{11, st_type::cast, "xor %q0, %q0 | mov %0, %d1", {AnyReg({p_size::DWORD}), RegMem({p_size::QWORD})}},
		{13, st_type::cast, "mov %0, 0 | mov %0, %b1", {Mem({p_size::BYTE}), AnyReg(WordS)}},
		{13, st_type::cast, "mov %0, 0 | mov %0, %w1", {Mem({p_size::WORD}), AnyReg(DWordS)}},
		{13, st_type::cast, "mov %0, 0 | mov %0, %d1", {Mem({p_size::DWORD}), AnyReg({p_size::QWORD})}},
		/* FCALL & RET - special */
		/* ADDROF */
		{10, st_type::addrof, "lea %0, %1", {AnyReg(AnyS), Mem(AnyS)}},
		/* STR */
		{10, st_type::str,  "lea %0, [rel __STRTABLE]", {AnyReg(AnyS)}}
	};

#undef AnyS      
#undef WordS    
#undef DWordS  
#undef SameAs
#undef Reg
#undef AnyReg
#undef RegMem 
#undef Any	 
#undef RegImm  
#undef Imm    
#undef Mem
#undef Const
#undef Ident  

	std::string storage::to_string(p_size rs) {
		switch (this->type) {
			case GLOBAL:
				switch (rs) {
					case p_size::BYTE:
						return "byte ptr [rel " + this->global + "]";
					case p_size::WORD:
						return "word ptr [rel " + this->global + "]";
					case p_size::DWORD:
						return "dword ptr [rel " + this->global + "]";
					case p_size::QWORD:
						return "qword ptr [rel " + this->global + "]";
					default:
						throw std::logic_error("Invalid size in to_string");
				}	
			case STACKOFFSET:
				switch (rs) {
					case p_size::BYTE:
						return "byte ptr [rbp - " + std::to_string(imm_or_offset) + "]";
					case p_size::WORD:
						return "word ptr [rbp - " + std::to_string(imm_or_offset) + "]";
					case p_size::DWORD:
						return "dword ptr [rbp - " + std::to_string(imm_or_offset) + "]";
					case p_size::QWORD:
						return "qword ptr [rbp - " + std::to_string(imm_or_offset) + "]";
					default:
						throw std::logic_error("Invalid size in to_string");
				}	
			case IMM:
				return std::to_string(imm_or_offset);
			case REG:
				switch (rs) {
					case p_size::BYTE:
						return registers[0][regno];
					case p_size::WORD:
						return registers[1][regno];
					case p_size::DWORD:
						return registers[2][regno];
					case p_size::QWORD:
						return registers[3][regno];
					default:
						throw std::logic_error("Invalid size in to_string");
				}
		}
	}

	std::string storage::to_string() {
		return to_string(get_size());
	}

	p_size storage::get_size() {
		if (this->type == IMM) return p_size::QWORD;
		switch (this->size.size) {
			case 8:
				return p_size::BYTE;
			case 16:
				return p_size::WORD;
			case 32:
				return p_size::DWORD;
			case 64:
				return p_size::QWORD;
			default:
				throw std::logic_error("Invalid size in to_string");
		}
	}

	bool storage::matches(const match_t &m) {
		switch (this->type) {
			case IMM:
				if (m.valid_types.count(match_t::IMM) + m.valid_types.count(match_t::CONSTIMM) == 0) return false;

				if (m.valid_types.count(match_t::CONSTIMM) == 1 && m.parm != imm_or_offset) return false;
				return true;
			case REG:
				if (m.valid_types.count(match_t::REG) == 0) return false;

				if (m.parm != ~0 && m.parm != regno) return false;
				if (m.valid_sizes.count(get_size()) == 0) return false;
				return true;
			case STACKOFFSET:
				if (m.valid_types.count(match_t::MEM) == 0) return false;

				if (m.valid_sizes.count(get_size()) == 0) return false;
				return true;
			case GLOBAL:
				if (m.valid_types.count(match_t::MEM) == 0) return false;

				if (m.valid_sizes.count(get_size()) == 0) return false;
				return true;
				break;
		}
	}

	codegenerator::codegenerator(tacoptimizecontext &&ctx) {
		func_compileunits = std::move(ctx.func_compileunits);
		global_initscope = std::move(ctx.global_initscope);
		string_table = std::move(ctx.string_table);
		ext_list = std::move(ctx.ext_list);

		current = nullptr;
	}

	std::string codegenerator::generate() {
		std::string output = generate_prologue();
		
		for (auto&[k, v] : func_compileunits) {
			output += k + ":\n";
			output += generate_unit(v);
		}

		// TODO: create the proper place for init.....
		
		output += output_string_table(string_table);
		output += output_bss_section();

		return output;
	}
}
