#include "codegen.h"
#include "flow.h"
#include "stringify.h"

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
#define MDWordS   {p_size::BYTE, p_size::WORD, p_size::DWORD}
#define MWordS    {p_size::BYTE, p_size::WORD}
#define SameAs(x) {{match_t::SAMEAS}, {}, x}
#define Reg(x, s) {{match_t::REG}, s, x}
#define AnyReg(s) {{match_t::REG}, s, ~0}
#define RegMem(s) {{match_t::REG, match_t::MEM}, s, ~0}
#define Any(s)	  {{match_t::REG, match_t::MEM, match_t::IMM}, s, ~0}
#define RegImm(s) {{match_t::REG, match_t::IMM}, s, ~0}
#define Imm(s)    {{match_t::IMM}, s, ~0}
#define Mem(s)    {{match_t::MEM}, s, ~0}
#define Const(x)  {{match_t::CONSTIMM}, AnyS, x}
#define Ident     {{match_t::IDENT}, AnyS, 0}

	const recipe recipes[] = {
		/* NOP */
		{10, st_type::nop, "nop", {}},
		/* MOV */
		{10, st_type::mov,  "mov %0, %1", {AnyReg(AnyS), RegImm(AnyS)}},
		{11, st_type::mov,  "mov %0, %1", {Mem(AnyS), AnyReg(AnyS)}},
		{11, st_type::mov,  "mov %0, %1", {Mem(AnyS), Imm(MDWordS)}},
		/* ADD */
		{ 9, st_type::add,  "inc %0", {RegMem(AnyS), SameAs(0), Const(1)}},
		{ 9, st_type::add,  "dec %0", {RegMem(AnyS), SameAs(0), Const(-1)}},
		{10, st_type::add,  "add %0, %2", {AnyReg(AnyS), SameAs(0), RegMem(AnyS)}},
		{11, st_type::add,  "add %0, %2", {Mem(AnyS), SameAs(0), AnyReg(AnyS)}},
		{10, st_type::add,  "add %0, %2", {RegMem(AnyS), SameAs(0), Imm(MDWordS)}},
		{ 8, st_type::add,  "lea %0, [%1 + %2]", {AnyReg(AnyS), AnyReg(AnyS), RegImm(AnyS)}},
		/* MUL */
		{10, st_type::mul,  "imul %0, %2", {AnyReg(AnyS), SameAs(0), RegMem(AnyS)}},
		{11, st_type::mul,  "imul %0, %1, %2", {AnyReg(AnyS), RegMem(AnyS), Imm(MDWordS)}},
		{ 9, st_type::mul,  "shl %0, 1", {RegMem(AnyS), SameAs(0), Const(2)}},
		{ 9, st_type::mul,  "shl %0, 2", {RegMem(AnyS), SameAs(0), Const(4)}},
		{ 9, st_type::mul,  "shl %0, 3", {RegMem(AnyS), SameAs(0), Const(8)}},
		/* DIV */
		{ 9, st_type::div,  "idiv %0 | xor ah, ah", {Reg(6, {p_size::BYTE}), SameAs(0), RegMem({p_size::BYTE})}},
		{10, st_type::div,  "cwd | idiv %2", {Reg(6, {p_size::WORD}), SameAs(0), RegMem({p_size::WORD})}, {2}},
		{10, st_type::div,  "cdq | idiv %2", {Reg(6, {p_size::DWORD}), SameAs(0), RegMem({p_size::DWORD})}, {2}},
		{10, st_type::div,  "cqo | idiv %2", {Reg(6, {p_size::QWORD}), SameAs(0), RegMem({p_size::QWORD})}, {2}},
		{ 8, st_type::div,  "shr %0, 1", {RegMem(AnyS), SameAs(0), Const(2)}},
		{ 8, st_type::div,  "shr %0, 2", {RegMem(AnyS), SameAs(0), Const(4)}},
		{ 8, st_type::div,  "shr %0, 3", {RegMem(AnyS), SameAs(0), Const(8)}},
        /* MOD */
		{ 9, st_type::mod,  "idiv %0 | shr ax, 8", {Reg(6, {p_size::BYTE}), SameAs(0), RegMem({p_size::BYTE})}},
		{10, st_type::mod,  "cwd | idiv %2", {Reg(2, {p_size::WORD}), Reg(6, {p_size::WORD}), RegMem({p_size::WORD})}},
		{10, st_type::mod,  "cdq | idiv %2", {Reg(2, {p_size::DWORD}), Reg(6, {p_size::DWORD}), RegMem({p_size::DWORD})}},
		{10, st_type::mod,  "cqo | idiv %2", {Reg(2, {p_size::QWORD}), Reg(6, {p_size::QWORD}), RegMem({p_size::QWORD})}},
		/* NEG */
		{10, st_type::neg,  "neg %0", {RegMem(AnyS), SameAs(0)}},
		{11, st_type::neg,  "imul %0, %1, -1", {AnyReg(AnyS), RegMem(AnyS)}},
		/* IFNZ */
		{10, st_type::ifnz, "cmp %0, 0 | jne %l", {RegMem(AnyS)}},
		/* IFEQ */
		{10, st_type::ifeq, "cmp %0, %1 | je %l", {RegMem(AnyS), RegImm(AnyS)}},
		{10, st_type::ifeq, "cmp %1, %0 | je %l", {RegImm(AnyS), RegMem(AnyS)}},
		/* IFGT */
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {RegMem(AnyS), AnyReg(AnyS)}},
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {RegMem(AnyS), Imm(MDWordS)}},
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {AnyReg(AnyS), RegMem(AnyS)}},
		/* READ */
		{10, st_type::read, "movzx %q0, byte [%1]", {AnyReg({p_size::BYTE}), AnyReg({p_size::BYTE})}},
		{10, st_type::read, "movzx %q0, word [%1]", {AnyReg({p_size::WORD}), AnyReg({p_size::WORD})}},
		{11, st_type::read, "xor %q0, %q0 | mov %0, dword [%1]", {AnyReg({p_size::DWORD}), AnyReg({p_size::DWORD})}},
		{11, st_type::read, "xor %q0, %q0 | mov %0, qword [%1]", {AnyReg({p_size::QWORD}), AnyReg({p_size::QWORD})}},
		/* WRITE */
		{10, st_type::write, "mov byte [%0], %1", {RegImm(AnyS), AnyReg({p_size::BYTE})}},
		{10, st_type::write, "mov word [%0], %1", {RegImm(AnyS), AnyReg({p_size::WORD})}},
		{10, st_type::write, "mov dword [%0], %1", {RegImm(AnyS), AnyReg({p_size::DWORD})}},
		{10, st_type::write, "mov qword [%0], %1", {RegImm(AnyS), AnyReg({p_size::QWORD})}},
		/* EQ */
		{10, st_type::eq,   "cmp %1, %2 | sete %0 ", {RegMem(AnyS), RegMem(AnyS), RegImm(AnyS)}},
		{10, st_type::eq,   "cmp %2, %1 | sete %0 ", {RegMem(AnyS), RegImm(AnyS), RegMem(AnyS)}},
		/* GT */
		{10, st_type::gt,   "cmp %1, %2 | seta %0 ", {RegMem(AnyS), RegMem(AnyS), RegImm(AnyS)}},
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

	std::string storage::to_string(p_size rs) const {
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

	std::string storage::to_string() const {
		return to_string(get_size());
	}

	p_size storage::get_size() const {
		if (this->type == IMM) {
			unsigned long ul = imm_or_offset;
			if (ul < 256u) return p_size::BYTE;
			if (ul < 65536u) return p_size::WORD;
			return p_size::DWORD;
		}
		switch (this->size) {
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

	bool storage::matches(const match_t &m) const {
		switch (this->type) {
			case IMM:
				if (m.valid_types.count(match_t::IMM) + m.valid_types.count(match_t::CONSTIMM) == 0) return false;

				if (m.valid_types.count(match_t::CONSTIMM) == 1 && m.parm != imm_or_offset) return false;
				if (m.valid_types.count(match_t::IMM) == 1 && m.valid_sizes.count(get_size()) == 0) return false;
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

	bool storage::matches(const match_t &m, const std::vector<storage> &others) const {
		if (m.valid_types.count(match_t::SAMEAS)) {
			return others[m.parm] == *this;
		}
		return matches(m);
	}

	bool storage::operator==(const storage& other) const {
		if (other.type != type) return false;
		switch (type) {
			case IMM: return imm_or_offset == other.imm_or_offset;
			case REG: return regno == other.regno && size == other.size;
			case STACKOFFSET: return imm_or_offset == other.imm_or_offset && size == other.size;
			case GLOBAL: return global == other.global;
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
		std::string output = /* generate_prologue() */ {};
		
		for (auto&[k, v] : func_compileunits) {
			output += k + ":\n";
			output += generate_unit(v);
		}

		// TODO: create the proper place for init.....
		
		//output += output_string_table(string_table);
		//output += output_bss_section();

		return output;
	}

	template<typename T, typename=void>
	struct emit_helper_ts_t {static const bool value = false;};
	template<typename T>
	struct emit_helper_ts_t<T, std::void_t<decltype(std::declval<T>().to_string())>> {static const bool value = true;};
	template<typename T> std::string emit_helper(const T& t) {
		if constexpr (std::is_constructible_v<std::string, T>)
			return std::string{t};
		else if constexpr (emit_helper_ts_t<T>::value)
			return t.to_string();
		else 
			return std::to_string(t);
	}
	struct emitter {
		emitter(std::string &result) : result(result) {}
		template<typename ...Args>
		void inline operator()(Args &&...args) {
			result += (emit_helper(std::forward<Args>(args)) + ...) + '\n';
		}
	private:
		std::string &result;
	};

	std::string codegenerator::generate_unit(compilation_unit &cu) {
		current = &cu;
		std::string result{};

		auto emit = emitter(result);
		
		// Allocate storage
		allocate_stores();

		// Emit prologue
		emit("push rbp");
		emit("mov rbp, rsp");
		if (local_stack_usage) emit("sub rsp, ", local_stack_usage);

		// Check if the storage allocator put any of the parameters on the stack
		
		for (long i = 0; i < cu.num_params; ++i) {
			if (stores[i].type == storage::STACKOFFSET) {
				// Generate an appropriate mov instruction.
				emit("mov ", stores[i], ", ", storage{(int)i, stores[i].size});
			}
		}

		// Now, using a similar method to the debug printer, construct a list of labels.
		
		auto labels = std::map<statement *, int>{};
		auto stmts  = traverse_v(cu.start);

		{
			auto add_label = [&, l=0](statement * i) mutable {
				if (labels.count(i) == 0) labels[i] = l++;
			};

			for (std::size_t i = 0; i < stmts.size(); ++i) {
				auto st = stmts[i];
				if (st->cond != nullptr) add_label(st->cond);
				if (st->next != nullptr && stmts[i+1] != st->next) add_label(st->next);
			}
		}	

		// Alright, we're now able to begin instruction conversion
		//
		
		for (std::size_t i = 0; i < stmts.size(); ++i) {
			const auto& stmt = stmts[i];
			// check if we need to emit a label?
			if (labels.count(stmt)) emit(".L", labels[stmt], ":");
			// assemble an instruction
			if (stmt->cond == nullptr)
				result += assemble(stmt); // assemble also handles fcall / ret with some special logic
			else
				result += assemble(stmt, labels[stmt->cond]); // assemble also handles fcall / ret with some special logic
			// if required, add a jmp
			if (stmt->next != nullptr && stmts[i+1] != stmt->next) {
				emit("jmp .L", labels[stmt->next]);
			}
		}

		return result;
	}
	
	// allocate_stores
	void codegenerator::allocate_stores() {
		enum req {
			MEMORY,
			ANY,
			PARAM
		};
		// Step 1: count all of the registers and their requirements
		std::map<long, std::set<req>> storage_requirements{};
		std::map<long, long> maximum_seen_size{};
		access_info info(*current, false, false);
		long maximum_register = 0;

		traverse_f(current->start, [&](statement *& stmt){
			// First check for MEMORY reqs
			if (si_addrof(*stmt)) {
				if (ai_reg(stmt->rhs())) {
					storage_requirements[stmt->rhs().num].insert(MEMORY);
					maximum_register = std::max(maximum_register, stmt->rhs().num);
				}
			}

			// Now, ensure the register count is up to date
			stmt->for_all_write([&](const addr_ref& ar){
				if (ai_reg(ar)) {
					storage_requirements[ar.num].insert(ANY);
					maximum_register = std::max(maximum_register, ar.num);
					maximum_seen_size[ar.num] = std::max(maximum_seen_size[ar.num], (long)ar.rt.size);
				}
			});

			// Update size for reads
			stmt->for_all_read([&](const addr_ref& ar){
				if (ai_reg(ar)) {
					maximum_seen_size[ar.num] = std::max(maximum_seen_size[ar.num], (long)ar.rt.size);
					storage_requirements[ar.num].insert(ar.num < current->num_params ? PARAM : ANY);
				}
			});
		});

		// Now we have a list of requirements, go and allocate the array.
		
		this->stores.clear(); this->stores.resize(maximum_register + 1);
		local_stack_usage = 0;

		// Go through all registers
		
		auto allocate = [&](long idx, auto &&...args) {
			this->stores[idx] = storage{std::forward<decltype(args)>(args)...};
		};

		int regno = 0;
		
		for (long i = 0; i <= maximum_register; ++i) {
			if (storage_requirements.count(i) == 0) continue;

			const auto& v = storage_requirements[i];
			if (v.count(MEMORY) > 0 || regno > 13) {
				// This needs to go onto the stack, so allocate another 
				local_stack_usage += maximum_seen_size[i] / 8;
				
				allocate(i, storage::STACKOFFSET, local_stack_usage + 8, (uint8_t)maximum_seen_size[i]);

				if (i < current->num_params) ++regno; // this could be improved: TODO optimize this stuff
			}
			else {
				allocate(i, regno++, (uint8_t)maximum_seen_size[i]);
			}
		}
	}

	

	std::string codegenerator::assemble(statement* stmt, int labelno) {
		if (si_fcall(*stmt) || si_ret(*stmt)) return assemble_special(stmt, labelno);

		// This happens in multiple phases:
		//  - First, attempt to find a perfect match. Pick the one with the least cost, and return
		//   
		//  - Otherwise, attempt to find the closest.
		//     - If any of the write parameters match, make the set of possibilities the set of those that match
		//       - If there are no write params, match on first read param
		//     - Otherwise, try all, but first
		//       - Match the write param:
		//         - If the write param is a _register_ and the target is a _register, use a mov (adding the write param to clobber set) and add one to the cost
		//         - If the write param is a _register_ and the target is _memory_, use a mov (add one to cost)
		//         - If the write param is _memory_, and the target is a _register_, use a mov (add _two_ to cost)
		//     - Now attempt to match all of the read parameters
		//       - In the case of a sameas:
		//         - Assemble a mov $2, $0 _UNLESS_ both are memory, in which case ALSO emit a mov $T, $0 where T is a scratch register (or in worst case, a clobberable, add 1 2 or 3 respectively)
		//       - If the target is supposed to be either a register or memory, assume register if there are scratch ones available, otherwise use memory (in case add 1)
		//       - If the target (after rule #2) is a _register_ or _memory_, and the current source is an _immediate_
		//         - If the immediate's size is 64 and _memory_ is to be used, remove this possibility
		//         - Otherwise, emit a mov %0, imm (add one to cost)
		//       - If the target is supposed to be _register_ and current source is the wrong register, use a mov (adding the target to the clobber set) and add one to cost
		//       - If the target is supposed to be _register_ and current source is _memory_, use a mov as well (adding one to cost)
		//       - If the target is supposed to be _memory_ and current source is _memory_, remove possibility
		//  
		//  - Next, handle clobbering.
		//    - For any register that is clobbered, do one of the following ONLY IF the register was allocated:
		//      - Are there available registers? If so, use mov to store, then use mov to restors
		//      - Otherwise, use push/pop
		//
		//  - Finally, add the resulting string.
		
		std::string result;
		auto emit = emitter(result);
		
		std::map<const recipe *, int> added_costs;
		auto consider_compare = [&](const recipe* a, const recipe* b){
			return std::make_tuple(a->cost + added_costs[a], a) < std::make_tuple(b->cost + added_costs[b], b);
		};
		std::vector<const recipe *> considered_set;

		// Grab the list of storages
		std::vector<storage> stores{};
		for (const auto &param : stmt->params) stores.push_back(get_storage_for(param));

		// Try and find matching combinations
		for (const auto& possible : recipes) {
			if (possible.type != stmt->t) continue;
			if (std::equal(stores.begin(), stores.end(), possible.matches.cbegin(), [&](const auto &a, const auto& b){
				return a.matches(b, stores);
			})) {
				considered_set.push_back(&possible);
			}
		}

		std::sort(considered_set.begin(), considered_set.end(), consider_compare);
		const recipe *chosen_recipe;
		std::string pre_command = "";
		std::string post_command = "";
		std::set<int> clobbers;

		std::set<int> ignored_registers;

		stmt->for_all_write([&](const auto& rf){
			storage st = get_storage_for(rf);
			if (st.type == storage::REG) {
				ignored_registers.insert(st.regno);
			}
		});

		if (!considered_set.empty()) {
			// Alright! Easy time!
			// No need to do _any_ fixing, so we can skip right to clobber generation
			chosen_recipe = considered_set.front();
			for (const auto &k : chosen_recipe->clobbers) clobbers.insert(k);
		}
		else {
			// Poo.
			// We need to begin the MAGICAL FIXING ALGORITHM TWO THOUSAND EDITION
			std::map<const recipe *, std::string> added_commands;
			std::map<const recipe *, std::string> post_commands;
			std::map<const recipe *, std::set<int>> added_registers;
			std::map<const recipe *, std::vector<storage>> chosen_stores;

			int num_write_params = 0; stmt->for_all_write([&](auto &){num_write_params = 1;});

			// Get all possibilites, so filtering later won't cause a starvation
			for (const auto& possible : recipes) {
				if (possible.type != stmt->t) continue;
				considered_set.push_back(&possible);
				chosen_stores[&possible] = stores;
			}

			std::sort(considered_set.begin(), considered_set.end(), consider_compare);

			// Get the list of important registers now.
			std::set<int> important_registers;
			stmt->for_all_read([&](const auto& rf){
				storage st = get_storage_for(rf);
				if (st.type == storage::REG) {
					important_registers.insert(st.regno);
				}
			});

			// Are there any matches?
			if (num_write_params) {
				// Now attempt to fill up the post_commands
				for (const auto& possible : considered_set) {
					const auto& wparam = possible->matches[0];
					if (stores[0].matches(wparam)) continue; // valid option
					if (wparam.valid_sizes.count(stores[0].get_size()) == 0) {
						// Invalid, mark as such by setting cost to -1
						added_costs[possible] = -1;
						continue;
					}
					post_commands[possible] = {}; // make sure it exists, since we use &
					auto emit = emitter(post_commands[possible]);
					if (wparam.valid_types.count(match_t::REG)) {
						if (stores[0].type == storage::REG) {
							// IN THIS CASE: we need to use a mov, because it is a register (if it was REGMEM it would have been matched earlier, therefore the only case is reg(:something)
							// Emit a mov
							emit("mov ", stores[0], ", ", storage{wparam.parm, stores[0].size});
							// Add 1 to cost
							added_costs[possible]++;
							// Add to clobber
							added_registers[possible].insert(wparam.parm);
							// Mark changed storage
							chosen_stores[possible][0] = storage{wparam.parm, stores[0].size};
						}	
						else if (stores[0].type == storage::STACKOFFSET || stores[0].type == storage::GLOBAL) {
							// IN THIS CASE: we need to use a mov, because it is memory, and if it was REGMEM it would have been matches.
							// The register, however, isn't always defined. If it isn't, find one.
							int reg = wparam.parm != ~0 ? wparam.parm : get_clobber_register(important_registers);
							// Add to clobber
							added_registers[possible].insert(reg);
							// Emit a mov
							emit("mov ", stores[0], ", ", storage{reg, stores[0].size});
							// Add _2_ to cost
							added_costs[possible] += 2;
							// Mark cosen storage location
							chosen_stores[possible][0] = storage{reg, stores[0].size};
						}	
						else {
							// Otherwise, it's an immediate, which is impossible, so throw an error.
							throw std::logic_error("Tried to write to an immed");
						}
					}
					else if (wparam.valid_types.count(match_t::MEM)) {
						// For now, simply disregard this, as there should _always_ be a vailid option with REG
						added_costs[possible] = -1;
						continue;
					}
				}

				// Discard invalid options
				considered_set.erase(std::remove_if(considered_set.begin(), considered_set.end(), [&](const auto &a){return added_costs[a] == -1;}), considered_set.end());
				// Sort valid options
				std::sort(considered_set.begin(), considered_set.end(), consider_compare);
			}

			if (considered_set.empty()) {
				// Still no options, give up and throw an error.
				throw std::logic_error("No remaining options");
			}

			// Alright! We've got some options now with which we've balanced the write parameter properly.

			for (const auto& possible : considered_set) {
				// Balance all of the read paramters.
				std::set<size_t> needs_remapping;
				auto emit = emitter(added_commands[possible]);
				for (size_t i = num_write_params; i < stores.size(); ++i) {
					if (needs_remapping.count(i)) {
						// This is a) a register, that needs to be changed. Add these instructions _without_ the emitter, since it needs to go before everything else.
						
						if (stores[i].matches(possible->matches[i])) {
							// Alright, all we have to do is allocate another register
							std::set<int> used_registers = important_registers; used_registers.merge(std::set<int>(added_registers[possible]));
							int newregno = get_clobber_register(used_registers);

							storage new_register{newregno, stores[i].size};
							added_commands[possible] = "mov ", new_register.to_string() + ", " + stores[i].to_string() + "\n" + added_commands[possible] ;
							added_registers[possible].insert(newregno);

							chosen_stores[possible][i] = std::move(new_register);
						}
						else {
							// We have issues: the thing is probably a memory, which could work. It also might be an imm, which won't.
							// For now, do a not-implemented
							
							added_costs[possible] = -1;
							break;
						}
					}
					if (stores[i].matches(possible->matches[i], chosen_stores[possible])) continue; // Alright, nothing to do.
					// otherwise, check the type of matches[i]
					if (possible->matches[i].valid_types.count(match_t::SAMEAS)) {
						// There's a sameas -- this almost _always_ is a sameas(0), so quickly make sure of that
						if (possible->matches[i].parm != 0) throw std::runtime_error("not implemented: !=0 sameas");
						// Otherwise, this means that we have to convert a ThrAC to a TwAC.
						// Do this by emitting a mov.
						if (chosen_stores[possible][0].type == storage::REG) {
							emit("mov ", chosen_stores[possible][0], ", ", stores[1]);
							// Mark chosen storage
							chosen_stores[possible][i] = chosen_stores[possible][0];
						}
						else if ((chosen_stores[possible][0].type == storage::STACKOFFSET || chosen_stores[possible][0].type == storage::GLOBAL) && (stores[i].type == storage::REG || stores[i].type == storage::IMM)) {
							// Also use a mov, but increase the cost
							emit("mov ", chosen_stores[possible][0], ", ", stores[1]);
							added_costs[possible]++;
							// Mark chosen storage
							chosen_stores[possible][i] = chosen_stores[possible][0];
						}
						else {
							// Mark as invalid
							added_costs[possible] = -1;
							break;
						}
					}
					// Check if it should be either register or memory and jump to appropriate block
					else if (possible->matches[i].valid_types.count(match_t::MEM) && possible->matches[i].valid_types.count(match_t::REG)) {
						if (is_clobber_available()) goto use_register;
						else 	                    goto use_mem;
					}
					else if (possible->matches[i].valid_types.count(match_t::REG)) {
use_register:
						std::set<int> used_registers = important_registers; used_registers.merge(std::set<int>(added_registers[possible]));
						int regno = possible->matches[i].parm != ~0 ? possible->matches[i].parm : get_clobber_register(used_registers);

						if (used_registers.count(regno)) {
							// Shuffling of the other read parameters is critical. Becuase of laziness, add a simple marker.
							for (size_t j = i + 1; j < stores.size(); ++j) {
								if (stores[j].type == storage::REG && stores[j].regno == regno) {
									needs_remapping.insert(j);
								}
							}
						}

						// Alright, now we have to go remap it. mov into the target.
						
						emit("mov ", storage{regno, stores[i].size}, ", ", stores[i]);

						chosen_stores[possible][i] = storage{regno, stores[i].size};
						added_costs[possible]++;
					}
					else {
use_mem:
						// For now, kill the possiblity
						added_costs[possible] = -1;
						break;
					}
				}
			}

			// Discard invalid options
			considered_set.erase(std::remove_if(considered_set.begin(), considered_set.end(), [&](const auto &a){
					// If the thing is _still_ not possibl,e also kill it with fire.
					if (!std::equal(chosen_stores[a].begin(), chosen_stores[a].end(), a->matches.cbegin(), [&](const auto &aa, const auto& b){
						return aa.matches(b, chosen_stores[a]);
					})) return true;

					return added_costs[a] == -1;
			}), considered_set.end());
			// Sort valid options
			std::sort(considered_set.begin(), considered_set.end(), consider_compare);

			if (considered_set.empty())
				throw std::logic_error("No more options");
			// pick chosen

			chosen_recipe = considered_set.front();

			// setup pre commands
			pre_command = added_commands[chosen_recipe];
			post_command = post_commands[chosen_recipe];
			
			// Add clobbers
			for (const auto &k : chosen_recipe->clobbers) clobbers.insert(k);
			for (const auto &k : added_registers[chosen_recipe]) clobbers.insert(k);

			// Change stores
			stores = chosen_stores[chosen_recipe];
		}

		// Finally, the two paths merge.
		
		for (const auto &k : clobbers) {
			if (ignored_registers.count(k)) continue; // ignore write params, as to not ruin the handiwork of the write solver
			// Fix the clobbering
			pre_command = "push " + storage{k, 64}.to_string() + '\n' + pre_command;
			post_command += "pop " + storage{k, 64}.to_string() + '\n';
		}

		// Now, begin emitting stuff
		
		result += pre_command;

		// Interpret the recipe.
		for (size_t i = 0; i < chosen_recipe->pattern.size();) {
			char c = chosen_recipe->pattern[i];
			switch (c) {
				case '|':
					result += '\n';
					while (chosen_recipe->pattern[++i] == ' ') {;}
					break;
				case '%':
					{
						++i;
						c = chosen_recipe->pattern[i];
						uint8_t size_override = 0;
						switch (c) {
							case 'l':
								result += ".L" + std::to_string(labelno);
								++i;
								continue;
							case 'b':
								size_override = 8;
								++i;
								break;
							case 'w':
								size_override = 16;
								++i;
								break;
							case 'd':
								size_override = 32;
								++i;
								break;
							case 'q':
								size_override = 64;
								++i;
								break;
							default:
								break;
						}
						int offset = chosen_recipe->pattern[i] - '0';

						// Substitude the recipe.
						result += (size_override ? storage{stores[offset], size_override} : stores[offset]).to_string();
						++i;
					}
					break;
				default:
					result += c;
					++i;
					break;
			}
		}

		result += '\n';

		result += post_command;

		return result;
	};

	storage codegenerator::get_storage_for(const addr_ref &ar) {
		// Is this AR a register?
		if (ai_reg(ar)) {
			return {stores[ar.num], static_cast<uint8_t>(ar.rt.size)};
		}
		else if (ai_num(ar)) {
			return {storage::IMM, ar.num};
		}
		else {
			return {ar.ident.name, static_cast<uint8_t>(ar.ident.t.size)};
		}
	}

	bool codegenerator::is_clobber_available() {
		int rcount;
		for (const auto &k : stores) {
			if (k.type == storage::REG) ++rcount;
		}
		return rcount < 14;
	}

	int codegenerator::get_clobber_register(std::set<int> regs) {
		std::set<int> used;
		for (const auto &k : stores) {
			if (k.type == storage::REG) used.insert(k.regno);
		}
		for (int regno = 13; regno >= 0; --regno) {
			if (used.count(regno) || regs.count(regno)) continue;
			return regno;
		}
		for (int regno = 13; regno >= 0; --regno) {
			if (regs.count(regno)) continue;
			return regno;
		}
		throw std::runtime_error("OUT OF REGISTERS OH NO POOOOOO");
	}

	std::string codegenerator::assemble_special(statement *s, int label) {
		return "; fcall/ret\n";
	} // TODO
}
