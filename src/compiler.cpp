#include "compiler.h"
#include "stringify.h"

compiler::compiler(astoptimizecontext &&ctx) : currently_compiling(&global_initscope) {
	for (auto &f : ctx.functions) {
		func_compileunits.emplace(std::string(f.name), std::move(f));	
	}
	// Create a new expression that will be used to initialize the globals
	auto make_set = [&](size_t i, expression &&e){
		return e_assign(e, identifier{id_type::global_var, i, ""});
	};

	if (ctx.global_inits.size() == 1) {
		global_initscope.e = make_set(0, std::move(ctx.global_inits[0]));
	}
	else if (ctx.global_inits.size() > 1) {
		global_initscope.e = e_comma();
		for (int i = 0; i < ctx.global_inits.size(); ++i) {
			global_initscope.e.params.emplace_back(std::move(make_set(i, std::move(ctx.global_inits[i]))));	
		}
	}
	ext_list = std::move(ctx.ext_functions);
}

void compiler::compile_all() {
	// Create a string table of all string constants
	create_string_table();
	// Compile every unit
	for (auto &[k, v] : func_compileunits) {
		compile_unit(v);
	}
	// Compile the global setup stuff
	compile_unit(global_initscope);
}

void compiler::create_string_table() {
	// Iterate over all expressions in all compile units
	std::vector<std::string> all_strings;

	auto push = [&](std::string s){
		all_strings.insert(std::lower_bound(all_strings.begin(), all_strings.end(), s, [](auto &l, auto&r){return l.size() < r.size();}), s);
	};

	for (auto &[k, v] : func_compileunits) {
		for_all_expr(v.e, true, is_string_ref, [&](expression &e){
			push(e.strvalue + '\0');		
		});
	}
	for_all_expr(global_initscope.e, true, is_string_ref, [&](expression &e){
		push(e.strvalue + '\0');		
	});

	// Remove duplications
	all_strings.erase(std::unique(all_strings.begin(), all_strings.end()), all_strings.end());	

	string_table.clear();
	for (auto &e : all_strings) 
		string_table += e;
}

void compiler::compile_unit(compilation_unit &c) {
	currently_compiling = &c;

	if (c.start != nullptr) {
		std::cerr << "attempted to recompile a compilation unit" << std::endl;
		return;
	}

	// Compile this units code
	this->compile(c.e);
}

addr_ref compiler::compile(const expression &expr) {
	// Define some simple lambdas
	//
	// append(statement *): put the statement chain onto the current tgt pointer
	// Sets the tgt pointer to the next poitner, and sets the parameter to deref of tgt, so tgt (next) = s, tgt = s->next.
	auto append = [&](statement *s) {for (*currently_compiling->tgt = s; s != nullptr; s = *currently_compiling->tgt) currently_compiling->tgt = &s->next; };
	// make() create a new unnamed register for expression usages
	auto make =   [&](ex_rtype &&t)	{return addr_ref(ar_type::reg, currently_compiling->counter++, std::forward<ex_rtype>(t));};

	addr_ref result{};

	switch (expr.t) {
		case ex_type::string_ref:
			append(s_str(result = make({})));
			append(s_add(result, result, addr_ref{ar_type::num, string_table.find(expr.strvalue + '\0')}));
			break;	
		case ex_type::ident:
			{
				switch (expr.ident.type) {
					case id_type::undefined: throw std::runtime_error("got undefined id");
					case id_type::extern_function:
					case id_type::function:
					case id_type::global_var:
						 result = addr_ref(expr.ident);
						 break;
					case id_type::local_var:
						 result = addr_ref(ar_type::reg, currently_compiling->num_params + expr.ident.index, expr.get_type());
						 break;
					case id_type::parameter:
						 result = addr_ref(ar_type::reg, expr.ident.index, expr.get_type());
						 break;
				}
				break;
			}
		case ex_type::literal_number:
		case ex_type::nop:
			result = addr_ref(ar_type::num, is_nop(expr) ? 0l : expr.numvalue);
			result.rt = {result.num < 256 ? 8u : 64u, false};
			break;
		case ex_type::deref:
			append(s_read(result   = make(expr.get_type()), compile(expr.params.front())));
			break;
		case ex_type::neg:
			append(s_neg(result    = make(expr.get_type()), compile(expr.params.front())));
			break;
		case ex_type::ret:
			append(s_ret(result    =         compile(expr.params.front())));
			break;
		case ex_type::addr:
			append(s_addrof(result = make(expr.get_type()), compile(expr.params.front())));
			break;

		case ex_type::add:
		case ex_type::eq:
		case ex_type::mul:
		case ex_type::div:
		case ex_type::comma:
		case ex_type::gt:
			// Reduce the parameters left to right into seperate stmts
			{
				for (auto &e : expr.params) {
					if (addr_ref prev = result, last = result = compile(e); prev.t != ar_type::unknown) {
						if (is_add(expr))      { append(s_add(result = make(expr.get_type()), prev, last)); }
						else if (is_mul(expr)) { append(s_mul(result = make(expr.get_type()), prev, last)); }
						else if (is_div(expr)) { append(s_div(result = make(expr.get_type()), prev, last)); }
						else if (is_eq(expr))  { append(s_eq(result = make(expr.get_type()), prev, last)); }
						else if (is_gt(expr))  { append(s_gt(result = make(expr.get_type()), prev, last)); }
						else                   { result = last; }
					}
				}
				break;
			}
		case ex_type::assign:
			{
				// Compile in the right order, and make sure we can do *a = write
				if (const auto& src = expr.params.front(), &dest = expr.params.back(); is_deref(dest)) {
					result = compile(src); append(s_write(compile(dest.params.front()), result));
				}
				else {
					auto temp = compile(src); append(s_mov(result = compile(dest), temp));
				}
				break;
			}
		case ex_type::fcall:
			{
				// First, compile the target expression that will be called
				std::vector<addr_ref> parameters{};
				parameters.push_back(result = make({}));
				for (auto i = expr.params.begin(); i != expr.params.end(); ++i) {
					parameters.emplace_back(std::move(compile(*i)));
				}

				// Create the s_fcall
				append(s_fcall(parameters));
				break;
			}
		// Aaaaaand now.... CONDITIONALS!!!! CRAP!!!!
		case ex_type::l_and:
		case ex_type::l_or:
		case ex_type::loop:
			{
				const bool is_and = !is_l_or(expr);

				result = make(expr.get_type());
				// Make three statements:
				// Then, else and a common target
				statement* b_then = s_mov(result, addr_ref{ar_type::num, is_and ? 1l : 0l});
				statement* b_else = s_mov(result, addr_ref{ar_type::num, is_and ? 0l : 1l});
				statement* end    = s_nop();

				// Tie then and else into the nop
				b_then->next = b_else->next = end;

				statement*& begin = *currently_compiling->tgt;
				bool b = true;
				for (auto &i : expr.params) {
					auto var = compile(i);
					if (is_loop(expr) && !b) continue;
					b = false;
					statement* condition = *currently_compiling->tgt = s_ifnz(var);
					if (is_and) { currently_compiling->tgt = &condition->cond; condition->next = b_else; }
					else        { currently_compiling->tgt = &condition->next; condition->cond = b_else; }
				} 
				*currently_compiling->tgt = is_loop(expr) ? begin : b_then;
				currently_compiling->tgt  = &end->next;
			}
			break;
		case ex_type::cast:
			{
				// Special stuff. First, check if the operand is a literal. We can store it directly in the desired type if so.
				if (is_literal_number(expr.params.front())) {
					// Simply generate a constant move into the result
					append(s_mov(result = make(std::move(ex_rtype(expr.castvalue))), addr_ref(ar_type::num, expr.params.front().numvalue, std::move(ex_rtype(expr.castvalue)))));
					break;
				}

				// Otherwise generate the cast instruction. This is pretty much the same as a mov but creates additional
				// code to handle the type switch
				append(s_cast(result = make(std::move(ex_rtype(expr.castvalue))), compile(expr.params.front())));
				break;
			}
	}
	
	if (result.t == ar_type::unknown) {
		std::cerr << "showing bad expr" << std::endl;
		print_tree(expr);
		throw std::runtime_error("encountered an unknown type result register.");
	}

	return result;
}
