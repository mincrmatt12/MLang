%skeleton "lalr1.cc"
%define parser_class_name {mlang_parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose
%locations
%code requires
{
#include <string>
#include <map>
#include <vector>
#include <list>
#include <algorithm>

#define ENUM_IDENTIFIER_TYPES(o) \
	o(undefined)		/* unknown type */ \
	o(function)		/* function pointer defined here (use label name) */ \
	o(extern_function)	/* external function (use func name + linker) */ \
	o(parameter)		/* function parameter (index into registers) */ \
	o(global_var)		/* global variable with respect to scope / global (static allocations) */ \
	o(local_var)		/* scope-local variable */
#define o(n) n,
enum class id_type { ENUM_IDENTIFIER_TYPES(o) };
#undef o

struct identifier {
	id_type type = id_type::undefined;
	std::size_t index = 0; // index for func/parameter/scopevar/globvar
	std::string name; // name + linker name for extern_function
};

#define ENUM_EXPRESSIONS(o) \
	o(nop) o(string_ref) o(literal_number) o(ident) /* nop, atomic types (char is converted to a literal number before here) */\
	o(add) o(neg) o(mul) o(div) o(eq)		/* arithmetic operators (sub is converted from neg + add) mul does not have a simpler repr, overrideable with smath_mul func */ \
	o(l_or) o(l_and) o(loop)			/* logical loop is while (param0) do param 1.. */ \
	o(addr) o(deref)				/* pointer arithmetic */ \
	o(fcall)					/* call param0 with param 1.. n */ \
	o(assign)					/* assign 0 to 1 */ \
	o(comma)					/* sequence, internal use */ \
	o(ret)						/* return p0 */

#define o(n) n,
enum class ex_type {ENUM_EXPRESSIONS(o)};
#undef o

typedef std::list<struct expression> expr_vec;
struct expression {
	ex_type t;
	identifier ident{};
	std::string strvalue{};
	long numvalue=0;
	expr_vec params;

	// function uses first parameter as function call target. the jump target is then eval'd later based on its type

	template<typename... T>
	expression(ex_type pt, T&&... args) : t(pt), params{std::forward<T>(args)...} {} // expand it out for varargs

	expression()			: t(ex_type::nop) {}
	expression(const identifier &i) : t(ex_type::ident), ident(i) {}
	expression(identifier &&i)	: t(ex_type::ident), ident(std::move(i)) {}
	expression(std::string &&s)	: t(ex_type::string_ref), strvalue(std::move(s)) {}
	expression(long v)		: t(ex_type::literal_number), numvalue(v) {}
	expression(char c)		: t(ex_type::literal_number), numvalue(static_cast<long>(c)) {}

	bool has_side_effects(bool typeonly=false) const;		// simple version, used for very basic checks during parsing
	// complex version, implemented in the ast optimizer and used during optimization checking
	bool is_compiletime_expr() const;
	expression operator%=(expression &&b) && { return expression(ex_type::assign, std::move(b), std::move(*this)); }
	bool operator==(const expression &e) const {
		return (e.t == t) 
			&& (t != ex_type::ident || (ident.type == e.ident.type && ident.index == e.ident.index))
			&& (t != ex_type::string_ref || strvalue == e.strvalue)
			&& (t != ex_type::literal_number || numvalue == e.numvalue)
			&& params == e.params;
	}
};

#define o(n) \
template<typename... T> \
inline expression e_##n(T&&... args) { \
	return expression(ex_type::n, std::forward<T>(args)...); \
}
ENUM_EXPRESSIONS(o)
#undef o

#define o(n) \
inline bool is_##n(const expression& e) {\
	return e.t == ex_type::n; \
}
ENUM_EXPRESSIONS(o)
#undef o

struct function {
	std::string name;
	expression code;
	unsigned int num_vars = 0; unsigned int num_args = 0; unsigned int num_globals = 0;
	std::vector<expression> global_initializers{};
};

struct ext_function {
	std::string name;
	unsigned int num_args = 0;
	bool varargs = false;
};

struct parsecontext;

struct var_decl_shim {
	std::string name;
	expression v;
};
}


%param {parsecontext &ctx } //param (allows it to take in extra parameters in the expansions)
%code provides
{

struct parsecontext
{
	const char* cursor;
	yy::location loc;
	int num_globals;
	std::map<std::string, identifier> global_ids; // should only ever contain global vars allocated at program scope & external funcs, and functions. temporaries allocated here should be handled in the global init stuff
	std::map<std::string, expression> global_initializers; // defaults to literal 0; used to generate global init code

	std::vector<function> func_list;
	std::vector<ext_function> ext_list;
	std::vector<std::map<std::string, identifier>> scopes; // contains ids at scope level-- global scoped etc.

	unsigned tempvar_count = 0; //creates temporary values
	function current_fun{}; // contains the current function being parsed
	ext_function current_ext{};
public:
	const identifier& define(const std::string& name, identifier && f) {
		auto r = (scopes.size() == 0 ? global_ids : scopes.back()).emplace(name, std::move(f));
		if (!r.second) throw yy::mlang_parser::syntax_error(loc, "Duplicate definition of " + name);
		return r.first->second;
	}

	inline bool parsing_function() const {return scopes.size() != 0;}

	expression defvar(const std::string& name) {return define(name, identifier{parsing_function() ? id_type::local_var : id_type::global_var, parsing_function() ? current_fun.num_vars++ : num_globals++, name});}
	expression defglobvar(const std::string& name)	{return define(name, identifier{id_type::global_var,       !parsing_function() ? num_globals++ : current_fun.num_globals++, name});}
	expression defun(const std::string& name)	{return define(name, identifier{id_type::function,         func_list.size(), name});}
	expression defexternal(const std::string& name)	{return define(name, identifier{id_type::extern_function,  ext_list.size(), name});} // 0 as external functions are pretty much magic references; taking pointers to them is illegal, etc.
	expression defparm(const std::string& name)	{
		if (!parsing_function()) {
			current_ext.num_args++;
			return e_nop();	
		}
		return define(name, identifier{id_type::parameter,        current_fun.num_args++, name});
	}

	void defvarargs() {
		if (parsing_function()) {
			throw yy::mlang_parser::syntax_error(loc, "Use of varargs in non-extern function definition.");
		}
		current_ext.varargs = true;
	}

	/* def* defines stuff */

	expression temp()				{return defvar("$T" + std::to_string(tempvar_count++));}

	expression use_name(const std::string& name) {
		for (auto j = scopes.crbegin(); j != scopes.crend(); ++j) {
			if (auto i = j->find(name); i != j->end()) {
				return i->second;
			}
		}
		if (auto i = global_ids.find(name); i != global_ids.end()) 
		{
			return i->second;
		}
		throw yy::mlang_parser::syntax_error(loc, "Use of id " + name + " before definition.");
	}

	void add_function(std::string &&name, expression&& code) {
		//if (code.t == ex_type::ret || (code.t == ex_type.comma && code.params))
		current_fun.code = e_comma(std::move(code), e_ret(0l)); // make sure the function always returns a 0
		current_fun.name = std::move(name);
		func_list.push_back(std::move(current_fun));
		current_fun = {};
	}

	void add_ext_function(std::string &&name) {
		current_ext.name = std::move(name);
		ext_list.push_back(std::move(current_ext));
		current_ext = {};
	}

	void operator++() {scopes.emplace_back();}
	void operator--() {scopes.pop_back();}
};}
%code
{

namespace yy {mlang_parser::symbol_type yylex(parsecontext &ctx); }

#define M(x) std::move(x)
#define C(x) expression(x)

} // end %code

%token END 0
%token RETURN "return" WHILE "while" IF "if" VAR "var" STATIC "static" EXTERN "extern" 
%token TRUE "true" FALSE "false" NULL_CONST "null"

%token OR "||" AND "&&" EQ "==" NE "!=" PP "++" MM "--" ADD_EQ "+=" SUB_EQ "-=" MUL_EQ "*=" DIV_EQ "/=" ELLIPSIS "..."
%token STR_CONST INT_LITERAL CHAR_LITERAL IDENTIFIER

%left ','
%right '?' ':' '=' "+=" "-=" "*=" "/="
%left "||"
%left "&&"
%left "==" "!="
%left '+' '-'
%left '/' '%'
%right '*' '&' "--" "++"
%left '(' '['

%type<long>		INT_LITERAL CHAR_LITERAL
%type<std::string>	IDENTIFIER STR_CONST
%type<expression> 	expr c_expr stmt comp_stmt var_def var_defs
%type<var_decl_shim>	var_decl
%%
library: defs;

defs: defs function
    | defs "extern" extern_function ';'
    | defs "static" var_decl {ctx.defglobvar($3.name); ctx.global_initializers.emplace(M($3.name), M($3.v));}';'
    | %empty;

function: IDENTIFIER {ctx.defun($1); ++ctx; } '(' paramdecls ')' '=' stmt {ctx.add_function(M($1), M($7)); --ctx;};
extern_function: IDENTIFIER {ctx.defexternal($1);}'(' paramdecls ')' {ctx.add_ext_function(M($1));};

paramdecls: paramdecl
	  | paramdecl ',' ELLIPSIS {ctx.defvarargs();}
	  | %empty;
paramdecl: paramdecl ',' IDENTIFIER {ctx.defparm($3);}
	 | IDENTIFIER {ctx.defparm($1);};

var_decl: IDENTIFIER '=' expr		{ $$ = {M($1), M($3)};}
	| IDENTIFIER			{ $$ = {M($1), 0l};};

stmt: comp_stmt '}'			{ $$ = M($1); --ctx;}
    | "if" '(' expr ')' stmt		{ $$ = e_l_and(M($3), M($5));}
    | "while" '(' expr ')' stmt		{ $$ = e_loop(M($3), M($5));}
    | "return" expr ';'			{ $$ = e_ret(M($2));}
    | var_defs ';'			{ $$ = M($1);}
    | expr ';'				{ $$ = M($1);}
    | ';'				{ $$ = e_nop();};

var_def: "static" var_decl		{ $$ = e_nop(); ctx.defglobvar($2.name); ctx.current_fun.global_initializers.emplace_back(M($2.v));}
	| "var" var_decl 		{ $$ = ctx.defvar(M($2.name)) %= M($2.v); };
var_defs: var_defs ',' var_def		{ $$ = M($1); $$.params.push_back(M($3));}
	| var_def			{ $$ = e_comma(M($1));};	
comp_stmt: '{'				{ ++ctx; $$ = e_comma();}
	 | comp_stmt stmt		{ $$ = M($1); $$.params.push_back(M($2));};

c_expr: c_expr ',' expr			{ $$ = M($1); $$.params.push_back(M($3));}
      | expr				{ $$ = e_comma(M($1));};

expr: STR_CONST				{ $$ = M($1);}
    | INT_LITERAL			{ $$ = $1;}
    | CHAR_LITERAL			{ $$ = $1;}
    | IDENTIFIER			{ $$ = ctx.use_name($1);}
    | "true"				{ $$ = 1l;}
    | "false"				{ $$ = 0l;}
    | "null"				{ $$ = 0l;}
    | '(' expr ')'			{ $$ = M($2);}
    | expr '[' expr ']'			{ $$ = e_deref(e_add(M($1), M($3)));}
    | expr '(' ')'			{ $$ = e_fcall(M($1));}
    | expr '(' c_expr ')'		{ $$ = e_fcall(M($1)); $$.params.splice($$.params.end(), M($3.params));}
    | expr '=' expr			{ $$ = M($1) %= M($3);}
    | expr '+' expr			{ $$ = e_add(M($1), M($3));}
    | expr '-' expr			{ $$ = e_add(M($1), e_neg(M($3)));}
    | expr '*' expr       %prec '/'	{ $$ = e_mul(M($1), M($3));}
    | expr '%' expr			{ if ($1.has_side_effects()) { $$ = ctx.temp() %= e_addr(M($1)); $1 = e_deref(C($$.params.back()));}
    				   	  if ($3.has_side_effects()) { $$ = e_comma(M($$), ctx.temp() %= e_addr(M($3))); $3 = e_deref($$.params.back().params.back());}	
					  $$ = e_comma(M($$), e_add(C($1), e_neg(e_mul(e_div(C($1), C($3)), M($3)))));    // calculate a % b with a + -((a / b) * b)
					}
    | expr '/' expr			{ $$ = e_div(M($1), M($3));}
    | expr "+=" expr			{ if ($1.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($1)); $$ = e_comma(C(a), e_add(e_deref(a.params.back()), M($3)) %= e_deref(a.params.back()));}
    					  else {$$ = C($1) %= e_add(M($1), M($3)); } }
    | expr "-=" expr			{ if ($1.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($1)); $$ = e_comma(C(a), e_add(e_deref(a.params.back()), e_neg(M($3))) %= e_deref(a.params.back()));}
    					  else {$$ = C($1) %= e_add(M($1), e_neg(M($3))); } }
    | expr "*=" expr			{ if ($1.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($1)); $$ = e_comma(C(a), e_mul(e_deref(a.params.back()), M($3)) %= e_deref(a.params.back()));}
    					  else {$$ = C($1) %= e_mul(M($1), M($3)); }}
    | expr "/=" expr			{ if ($1.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($1)); $$ = e_comma(C(a), e_div(e_deref(a.params.back()), M($3)) %= e_deref(a.params.back()));}
    					  else {$$ = C($1) %= e_div(M($1), M($3)); } }
    | expr "||" expr			{ $$ = e_l_or(M($1), M($3));}
    | expr "&&" expr			{ $$ = e_l_and(M($1), M($3));}
    | expr "==" expr			{ $$ = e_eq(M($1), M($3)); }
    | expr "!=" expr			{ $$ = e_eq(e_eq(M($1), M($3)), 0l);}
    | '&' expr				{ $$ = e_addr(M($2));}
    | '*' expr            %prec '&'	{ $$ = e_deref(M($2));}
    | '-' expr            %prec '&'	{ $$ = e_neg(M($2));}
    | '!' expr            %prec '&'	{ $$ = e_eq(M($2), 0l);}
    | "++" expr				{ if ($2.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($2)); $$ = e_comma(C(a), e_add(e_deref(a.params.back()), 1l)) %= e_deref(a.params.back());}
    					  else {$$ = C($2) %= e_add(M($2), 1l); }}	
    | "--" expr           %prec "++"	{ if ($2.has_side_effects()) {auto a = ctx.temp() %= e_addr(M($2)); $$ = e_comma(C(a), e_add(e_deref(a.params.back()), -1l)) %= e_deref(a.params.back());}
    					  else {$$ = C($2) %= e_add(M($2), -1l); }}
    | expr "++"				{ if ($1.has_side_effects()) { $$ = ctx.temp() %= e_addr(M($1)); $1 = e_deref($$.params.back());}
    					  auto t = ctx.temp(); $$ = e_comma(M($$), C(t) %= C($1), C($1) %= e_add(C($1), 1l), M(t)); }
    | expr "--"           %prec "++"	{ if ($1.has_side_effects()) { $$ = ctx.temp() %= e_addr(M($1)); $1 = e_deref($$.params.back());}
    					  auto t = ctx.temp(); $$ = e_comma(M($$), C(t) %= C($1), C($1) %= e_add(C($1), e_neg(1l)), M(t)); }
    | expr '?' expr ':' expr		{ auto t = ctx.temp(); $$ = e_comma(e_l_or(e_l_and(M($1), e_comma(C(t) %= M($3), 1l)), C(t) %= M($5)), C(t));};
%%

yy::mlang_parser::symbol_type yy::yylex(parsecontext& ctx) {
	const char* anchor = ctx.cursor;
	ctx.loc.step();
	const char * re2c_marker;
	auto s = [&](auto func, auto&&... params){ctx.loc.columns(ctx.cursor - anchor); return func(params..., ctx.loc);};
	#define tk(t, ...) s(yy::mlang_parser::make_##t, ##__VA_ARGS__)
%{
// re2c lexer here:
// note that spaces are ignored in the regex thing
re2c:yyfill:enable	= 0;
re2c:define:YYCTYPE	= "char";
re2c:define:YYCURSOR	= "ctx.cursor";
re2c:define:YYMARKER	= "re2c_marker";

// Keywords

"return"		{ return tk(RETURN); }	
"while"			{ return tk(WHILE); }
"var"			{ return tk(VAR); }
"static"		{ return tk(STATIC); }
"extern"		{ return tk(EXTERN); }

// Constants

"true"			{ return tk(TRUE); }
"false"			{ return tk(FALSE); }
"null"			{ return tk(NULL_CONST); }

// Identifier

[a-zA-Z_] [a-zA-Z_0-9]*	{ return tk(IDENTIFIER, std::string(anchor, ctx.cursor)); }

// Literals

"\"" [^"]* "\""		{ return tk(STR_CONST, std::string(anchor+1, ctx.cursor-1)); }
"'" [^'] "'"		{ return tk(CHAR_LITERAL, static_cast<long>(*(anchor+1))); }
[0-9]+			{ return tk(INT_LITERAL,  std::stol(std::string(anchor, ctx.cursor))); }

// Whitespace and ignored things

"\000"			{ return tk(END); }
"\r\n" | [\r\n]		{ ctx.loc.lines();	return yylex(ctx); }
"//" [^\r\n]*		{ 			return yylex(ctx); }
[\t\v\b\f ]		{ ctx.loc.columns();	return yylex(ctx); }

// Operators

"&&"			{ return tk(AND); }
"||"			{ return tk(OR); }
"=="			{ return tk(EQ); }
"!="			{ return tk(NE); }
"++"			{ return tk(PP); }
"--" 			{ return tk(MM); }
"+="			{ return tk(ADD_EQ); }
"-="			{ return tk(SUB_EQ); }
"*="			{ return tk(MUL_EQ); }
"/="			{ return tk(DIV_EQ); }
"..."			{ return tk(ELLIPSIS); }

// Invalid

.			{ return s([](auto... s){return mlang_parser::symbol_type(s...);}, mlang_parser::token_type(ctx.cursor[-1]&0xFF)); }

%}
	#undef tk
}

void yy::mlang_parser::error(const location_type &l, const std::string &m) {
	std::cerr << (l.begin.filename ? l.begin.filename->c_str() : "(undefined)");
	std::cerr << ':' << l.begin.line << ':' << l.begin.column << '-' << l.end.column << ": " << m << '\n';
}

// has_side_effects: does evaluating this expression write or modify anything?

bool expression::has_side_effects(bool typeonly) const {
	// if any parameter has side effects, then the expression is bad
	for (const auto &e : this->params) if (e.has_side_effects() && !typeonly) return true;

	switch (t) {
		// assigns write; has side effect
		case ex_type::assign:	return true;
		// function calls depend on various things, and since we don't know what
		// the function could contain (external etc) we assume it has side effects
		case ex_type::fcall: 	return true;
		// return jumps around, so it has a side effect
		case ex_type::ret:	return true;
		// loops could become stuck, which means they have a side effect
		case ex_type::loop:	return true;
		// anything else is fine though
		default:		return false;
	}
}

bool expression::is_compiletime_expr() const {
	for (const auto &e : this->params) if (!e.is_compiletime_expr()) return false;

	switch (t) {
		case ex_type::string_ref:	case ex_type::literal_number:
		case ex_type::add:		case ex_type::neg: 	case ex_type::mul:
		case ex_type::div:		case ex_type::l_or:	case ex_type::l_and:
		case ex_type::nop:		case ex_type::comma:
			return true;
		case ex_type::ident:
			return ident.type == id_type::function;
		default:
			return false;
	}
}
