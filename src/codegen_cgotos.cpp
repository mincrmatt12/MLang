#include "codegen.h"
#include <sstream>

namespace cgotos {
	std::string encode_string(const std::string &str_table) {
		std::stringstream result;
		for (char c : str_table) {
			if (c == '\n') result << "\\n";
			else if (c == '\t') result << "\\t";
			else if (c == '"') result << "\\\"";
			else if (c == '\f') result << "\\f";
			else if (c == '\\') result << "\\\\";
			else if (isprint(c)) result << c;
			else {
				result << std::oct << "\\" << (c < 0100 ? "0" : "") << (c < 010 ? "0" : "") << int{c};
			}
		}
		return '"' + result.str() + '"';
	}

	std::string codegenerator::ctypeof(const ex_rtype& er) {
		if (er.ptr) return ctypeof(*er.ptr) + "*";
		else {
			switch (er.size) {
				case 8: return "char";
				case 16: return "short";
				case 32: return "int";
				default: return "long";
			}
		}
	}

	std::string codegenerator::cnameof(const addr_ref &ar) {
		switch (ar.t) {
			case ar_type::reg:
				if (current->register_types[ar.num].count(ar.rt) == 0) throw std::runtime_error("no value");
				if (current->register_types[ar.num].size() == 1) return current->register_types[ar.num][ar.rt];
				else {
					return "_r" + std::to_string(ar.num) + "." + current->register_types[ar.num][ar.rt];
				}
			case ar_type::ident:
				return ar.ident.name;
			case ar_type::num:
				return std::to_string(ar.num);
			case ar_type::unknown:
				throw std::runtime_error("unk cnameof");
		}
		return "<unk>";
	}

	codegenerator::codegenerator(tacoptimizecontext &&tctx) {
		for (auto&& [nm, f] : tctx.func_compileunits) {
			this->functions.emplace_back(std::move(f), nm);
		}

		this->string_table = encode_string(tctx.string_table);
	}

	std::string codegenerator::generate() {
		// extract typeinfo for all funcs
		for (auto& f : functions)
			extract_types(f);

		std::string output = "";

		for (auto& f : functions)
			output += generate_function_header(f) + ";\n";

		output += "\n\n";

		for (auto& f : functions)
			output += generate_function(f);

		return output;
	}

	std::string codegenerator::generate_function_header(const declared_function &f, bool use_names) {
		std::string signature = "";

		signature += ctypeof(f.cu.e.get_type()) + " " + f.name + "(";
		for (int i = 0; i < f.cu.num_params; ++i) {
			signature += ctypeof(f.cu.parameter_types[i]) + " " + (use_names ? f.argnames[i] : ("_p" + std::to_string(i)));
			if (i != f.cu.num_params-1) signature += ", ";
		}

		return signature + ")";
	}

	void codegenerator::generate_vardecls(declared_function &f, emitter& emit) {
		for (int i = 0; i < f.register_types.size(); ++i) {
			// generate type
			std::string typedf = "", totalnm = "";
			if (f.register_types[i].empty()) continue;
			else if (f.register_types[i].size() == 1) {
				typedf = ctypeof(f.register_types[i].begin()->first);
				totalnm = f.register_types[i].begin()->second;
			}
			else {
				typedf = "union { ";
				for (auto &[rt, rn] : f.register_types[i]) {
					typedf += ctypeof(rt) + " " + rn + "; ";
				}
				typedf += "}";
				totalnm = "_r" + std::to_string(i);
			}

			if (i >= f.cu.num_params) {
				emit(typedf, " ", totalnm, ";");
			}
			else {
				std::string init = f.register_types[i].size() == 1 ? "_p" + std::to_string(i) : ("{." + f.register_types[i][f.cu.parameter_types[i]] + " = _p" + std::to_string(i) + "}");
				emit(typedf, " ", totalnm, " = ", init, ";");
			}
		}
	}

	std::string codegenerator::generate_function(declared_function &f) {
		current = &f;

		// create header
		std::string result = generate_function_header(f, false) + " {\n";
		auto emit = emitter(result, "    ");
		auto emitnoindent = emitter(result);

		generate_vardecls(f, emit);
		emit();

		auto [stmts, labels] = traverse_l(f.cu.start);

		for (int i = 0; i < stmts.size(); ++i) {
			auto &stmt = stmts[i];
			// do we need a label?
			if (labels.count(stmt)) emitnoindent("L", labels[stmt], ":");
			// do instruction
			if (stmt->cond) generate_stmt(stmt, emit, "L" + std::to_string(labels[stmt->cond]));
			else            generate_stmt(stmt, emit);
			// if next insn is not next, add goto
			if (stmt->next != nullptr && stmts[i+1] != stmt->next) {
				emit("goto L", labels[stmt->next], ";");
			}
		}

		result += "}\n";

		return result;
	}

	std::string operatorfor(const st_type &s) {
		switch (s) {
			case st_type::add:  return " + ";
			case st_type::mul:  return " * ";
			case st_type::div:  return " / ";
			case st_type::mod:  return " % ";
			case st_type::eq:   return " == ";
			case st_type::gt:   return " > ";
			case st_type::ifeq: return " == ";
			case st_type::ifgt: return " > ";
			default:
				throw std::domain_error("invalid s for operator");
		}
	}

	void codegenerator::generate_stmt(statement *stmt, emitter& emit, std::string condlabel) {
		switch (stmt->t) {
			case st_type::nop:
				emit("; // nop"); // nop
				break;
			case st_type::mov:
				emit(cnameof(stmt->lhs()), " = ", cnameof(stmt->rhs()), ";");
				break;
			case st_type::add:
			case st_type::mul:
			case st_type::div:
			case st_type::mod:
			case st_type::eq:
			case st_type::gt:
				emit(cnameof(stmt->lhs()), " = ", cnameof(stmt->params[1]), operatorfor(stmt->t), cnameof(stmt->params[2]), ";");
				break;
			case st_type::neg:
				emit(cnameof(stmt->lhs()), " = -", cnameof(stmt->rhs()), ";");
				break;
			case st_type::ifnz:
				emit("if (", cnameof(stmt->lhs()), ") goto ", condlabel, ";");
				break;
			case st_type::ifeq:
			case st_type::ifgt:
				emit("if (", cnameof(stmt->lhs()), operatorfor(stmt->t), cnameof(stmt->rhs()), ") goto ", condlabel, ";");
				break;
			case st_type::read:
				emit(cnameof(stmt->lhs()), " = *(", cnameof(stmt->rhs()), ");");
				break;
			case st_type::write:
				emit("*(", cnameof(stmt->lhs()), ") = ", cnameof(stmt->rhs()), ";");
				break;
			case st_type::fcall:
				{
					// todo: void functions
					std::string line = cnameof(stmt->lhs()) + " = " + cnameof(stmt->params[1]) + "(";
					for (int i = 2; i < stmt->params.size(); ++i) {
						line += cnameof(stmt->params[i]);
						if (i != stmt->params.size()-1) line += ", ";
					}
					emit(line, "); // TODO: check for void");
				}
				break;
			case st_type::ret:
				emit("return ", cnameof(stmt->lhs()), ";");
				break;
			case st_type::addrof:
				emit(cnameof(stmt->lhs()), " = &(", cnameof(stmt->rhs()), ");");
				break;
			case st_type::str:
				emit(cnameof(stmt->lhs()), " = ", this->string_table, ";");
				break;
			case st_type::stackoff:
				throw std::logic_error("no stackoffs");
			case st_type::cast:
				emit(cnameof(stmt->lhs()), " = (", ctypeof(stmt->lhs().rt), ")(", cnameof(stmt->rhs()), ");");
				break;
		}
	}

	void codegenerator::extract_types(declared_function &df) {
		// record all types for a variable (to be used in unions)
		
		df.register_types.resize(df.cu.counter);
		df.argnames.resize(df.cu.num_params);

		for (auto& stmt : traverse_v(df.cu.start)) {
			for (auto& ar : stmt->params) {
				if (ai_reg(ar)) {
					if (ar.num < df.cu.num_params && ar.has_associated_name && df.argnames[ar.num] == "") df.argnames[ar.num] = ar.associated_name;
					if (df.register_types[ar.num].count(ar.rt) > 0) continue;
					std::string thename = (ar.has_associated_name && ar.associated_name != "" ? ar.associated_name : "_v" + std::to_string(ar.num));
					if (df.register_types[ar.num].size() != 0) thename += std::to_string(df.register_types[ar.num].size());
					df.register_types[ar.num][ar.rt] = thename;
				}
			}
		}
	}
}
