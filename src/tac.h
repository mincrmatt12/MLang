// tac.h
//
// Contains types related to ThreeAddressCode
//
// Otherwise known as IR-bytecode to the likes of minimatts
// :)
// 
// Nearly all of the code references use "registers"
//
// Registers refer to locally accessible values. These can be temporaries created during expression evaluation, or can be locals.
// Global variables are referred to using the same datatype, <addr-ref> but instead of having a number they have an identifier tied
// to them. The same holds for external functions and normal functions. In the other cases the fcall statement just takes a register,
// and if the register contains a function/extfunction different machine code is generated to handle it. This is because we let the
// assembler decide the actual physical locations of these things.
// The struct addr_ref contains either the register number (temporaries only or local vars or parameters)

// Define the addr_ref

#ifndef TAC_H
#define TAC_H
#include <parser.h>
#include <queue>
#include <unordered_set>

#define ENUM_ADDR_REFS(o) \
	o(reg) o(num) o(ident)

#define o(n) n, 
enum struct ar_type {
	ENUM_ADDR_REFS(o) unknown
};
#undef o

struct addr_ref {
	ar_type t{ar_type::unknown};
	long num=0;
	identifier ident{};
	ex_rtype rt{64, false};
	
	addr_ref() = default;

	template<typename ...T>
	addr_ref(ar_type t_, T&& ...p) : addr_ref(std::forward<T>(p)...)  { t = t_;} 
	
	addr_ref(long n) : num(n) {}
	addr_ref(long n, ex_rtype &&t) : num(n), rt(t) {}
	addr_ref(identifier i) : ident(i) {t = ar_type::ident; rt = ex_rtype(i.t);}
};

#define o(n) \
inline bool ai_##n(const addr_ref &e) { \
	return e.t == ar_type::n; \
}
ENUM_ADDR_REFS(o)
#undef o

// Now, define the different statement or instruction types
// Named resources such as functions and extfunctions and global variables
// are reffered to with mov statements. Offsets are done with dumb adds

#define ENUM_STATEMENTS(o) \
	o(nop) /* do nothing */ \
	o(mov) /* p0 <- p1 copy a register to another register. */ \
	o(add) o(mul) o(div) /* p0 <- p1 @ p2 do arithmetic on register to another register */ \
	o(neg) /* p0 <- -p1 */ \
	o(ifnz)	/* if not zero, jump */ \
	o(read) o(write) /* read/write derefs */ \
	o(eq) o(gt) \
	o(fcall) o(ret) \
	o(addrof) /* get the physical address of p1 and put it into p0. should be implemented with a simple constant store at the compile level */ \
	o(str) /* kludge to get a pointer to the string table and place it into the target. if constants are added they will be stored with this type 
		  after it is renamed */ \
	o(cast)

#define o(n) n, 
enum struct st_type {
	ENUM_STATEMENTS(o)
};
#undef o

struct statement {
	st_type 		t{st_type::nop};
	std::vector<addr_ref>	params{};	
	statement*		next{nullptr};
	statement*		cond{nullptr};

	template<typename... T>
	statement(st_type t_, T&&... args) : t(t_), params{std::forward<T>(args)...} {}

	statement(st_type t_, std::vector<addr_ref> args) : t(t_) {params = std::move(args);}
};

#define o(n) \
inline bool si_##n(const statement &e) { \
	return e.t == st_type::n; \
}
ENUM_STATEMENTS(o)
#undef o

//
// Allocation of registers requires access to a compilation unit which knows how many local variables and parameters there are. (params take up 0-nparams, 
// local vars nparams-nparams+nlocals, rest are temporaries.
// The compilation unit also contains the expression to be compiled. It also contains a pointer to the start of the compiled expression as a statement.

struct compilation_unit {
	expression e{};
	statement * start{nullptr};
	statement ** tgt;
	unsigned num_params = 0; unsigned num_locals = 0;
	unsigned counter = 0;
	
	compilation_unit() : tgt(&start) {};

	compilation_unit(function &&n) {
		e = std::move(n.code);
		num_params = std::move(n.num_args);
		num_locals = std::move(n.num_vars);
		counter = num_params + num_locals;
		tgt = &start;
	}

	compilation_unit(expression &&express) {
		e = std::move(express);	
		tgt = &start;
	}
};

// std::map<stmt *, int> traverse(stmt *):
//
// Creates a map of statements to indices representing the order in which they will be written in a stream
// Traverses all next pointers first followed by cond pointers in a breadth first system
static std::map<statement *, int> traverse(statement *s) {
	std::map<statement *, int> result{};
	std::priority_queue<std::pair<int, statement *>> to_read{};
	std::unordered_set<statement *> visited{};
	to_read.push({2, s});
	int index = 0;

	while (!to_read.empty()) {
		auto [_, v] = to_read.top();
		if (v->t == st_type::ret && v->next != nullptr) std::cerr << "AAAAAAA" << std::endl;
		if (visited.count(v) != 0) {to_read.pop(); continue;}
		visited.insert(v);
		result[v] = index++;
		to_read.pop();

		if (v->next != nullptr) to_read.push({1, v->next});
		if (v->cond != nullptr) to_read.push({0, v->cond});
	}

	return result;
}

#endif
