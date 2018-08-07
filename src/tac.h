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

#include <parser.h>

#define ENUM_ADDR_REFS(o) \
	o(reg) o(num) o(ident)

#define o(n) n, 
enum struct ar_type {
	ENUM_ADDR_REFS(o)
};
#undef o

struct addr_ref {
	long num;
	identifier ident{};
	ar_type t;
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
	o(eq) \
	o(fcall) o(ret) \
	o(addrof) /* get the physical address of p1 and put it into p0. should be implemented with a simple constant store at the compile level */

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
	statement(st_type t, T&&... args) : t(t), params{std::forward<T>(args)...} {}
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
	expression e;
	statement * start;
	unsigned num_params; unsigned num_locals;
};
