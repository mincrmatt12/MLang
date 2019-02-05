#include "codegen.h"
#include "flow.h"

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
		{10, st_type::ifeq, "cmp %0, %1 | je %l", {RegMem(AnyS), RegImm(MDWordS)}},
		{10, st_type::ifeq, "cmp %1, %0 | je %l", {RegImm(MDWordS), RegMem(AnyS)}},
		/* IFGT */
		{10, st_type::ifgt, "cmp %0, %1 | ja %l", {RegMem(AnyS), RegImm(MDWordS)}},
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
		{10, st_type::eq,   "cmp %1, %2 | sete %0 ", {RegMem(AnyS), RegMem(AnyS), RegImm(MDWordS)}},
		{10, st_type::eq,   "cmp %2, %1 | sete %0 ", {RegMem(AnyS), RegImm(MDWordS), RegMem(AnyS)}},
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
			ANY
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
				}
			});
		});

		// Now we have a list of requirements, go and allocate the array.
		
		this->stores.clear(); this->stores.resize(maximum_register);
		local_stack_usage = 0;

		// Go through all registers
		
		auto allocate = [&](long idx, auto &&...args) {
			this->stores[idx] = storage{std::forward<decltype(args)>(args)...};
		};

		int regno = 0;
		
		for (long i = 0; i < maximum_register; ++i) {
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
			if (std::equal(stores.begin(), stores.end(), possible.matches.cbegin(), [](const auto &a, const auto& b){
				return a.matches(b);
			})) {
				considered_set.push_back(&possible);
			}
		}

		std::sort(considered_set.begin(), considered_set.end(), consider_compare);
		const recipe *chosen_recipe;

		if (!considered_set.empty()) {
			// Alright! Easy time!
			// No need to do _any_ fixing, so we can skip right to clobber generation
			chosen_recipe = considered_set.front();
		}
		else {
			// Poo.
			// We need to begin the MAGICAL FIXING ALGORITHM TWO THOUSAND EDITION
			std::map<const recipe *, std::string> added_commands;
			std::map<const recipe *, std::string> post_commands;
			std::map<const recipe *, std::set<int>> added_registers;

			int num_write_params = 0; stmt->for_all_write([&](auto &){num_write_params = 1;});

			if (num_write_params) {
				for (const auto& possible : recipes) {
					if (possible.type != stmt->t) continue;
					if (stores[0].matches(possible.matches[0])) considered_set.push_back(&possible);
				}
			}

			std::sort(considered_set.begin(), considered_set.end(), consider_compare);

			// Are there any matches?
			if (!considered_set.empty()) {
				// I guess not... if there _was_ a write parameter, we need to match it up. Do that for all of them. Otherwise, we just continue on with all possiblities.
				for (const auto& possible : recipes) {
					if (possible.type != stmt->t) continue;
					considered_set.push_back(&possible);
				}

				// Get the list of important registers now.
				std::set<int> important_registers;
				stmt->for_all_read([&](const auto& rf){
					storage st = get_storage_for(rf);
					if (st.type == storage::REG) {
						important_registers.insert(st.regno);
					}
				});

				if (num_write_params) {
					// Now attempt to fill up the post_commands
					for (const auto& possible : considered_set) {
						const auto& wparam = possible->matches[0];
						if (wparam.valid_sizes.count(stores[0].get_size()) == 0) {
							// Invalid, mark as such by setting cost to -1
							added_costs[possible] = -1;
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
							}	
							else if (stores[0].type == storage::STACKOFFSET || stores[0].type == storage::GLOBAL) {
								// IN THIS CASE: we need to use a mov, because it is memory, and if it was REGMEM it would have been matches.
								// The register, however, isn't always defined. If it isn't, find one.
								int reg = wparam.parm == ~0 ? wparam.parm : get_clobber_register(important_registers);
								// Add to clobber
								added_registers[possible].insert(reg);
								// Emit a mov
								emit("mov ", stores[0], ", ", storage{reg, stores[0].size});
								// Add _2_ to cost
								added_costs[possible] += 2;
							}	
							else {
								// Otherwise, it's an immediate, which is impossible, so throw an error.
								throw std::logic_error("Tried to write to an immed");
							}
						}
						else if (wparam.valid_types.count(match_t::MEM)) {
							// For now, simply disregard this, as there should _always_ be a vailid option with REG
							added_costs[possible] = -1;
						}
					}

					// Discard invalid options
					considered_set.erase(std::remove_if(considered_set.begin(), considered_set.end(), [&](const auto &a){return added_costs[a] == -1;}), considered_set.end());
					// Sort valid options
					std::sort(considered_set.begin(), considered_set.end(), consider_compare);
				}
			}

			if (considered_set.empty()) {
				// Still no options, give up and throw an error.
				throw std::logic_error("No remaining options");
			}

			// Alright! We've got some options now with which we've balanced the write parameter properly.
		}
	};
}
