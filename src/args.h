// args.h - contains commandline argument parsing functions

// Laziness entails that all arguments are packed together

// Store a massive list of all optimization techniques

#ifndef ARGS_H
#define ARGS_H

#include <vector>
#include <stdexcept>
#include <algorithm>
#include <string>

extern std::vector<bool> optimize;

// o(o1, o2, o3, name)
#define OPTIMIZE_FLAGS(o) \
o(1, 1, 1, flatten_tree, 0) \
o(1, 1, 1, remove_ast_deadcode, 1) \
o(1, 1, 1, ast_arith_constfold, 2) \
o(1, 1, 1, ast_logic_constfold, 3) \
/* o(1, 1, ast_simplifycasts) */ \
o(1, 1, 1, remove_nops, 4) \
o(0, 1, 1, remove_deadstores, 5) \
o(0, 0, 1, deduplicate, 6) \
o(0, 1, 1, copy_elide_read, 7) \
o(0, 0, 1, copy_elide_write, 8) \
o(0, 1, 1, simplify_useless_casts, 9) \
o(0, 1, 1, replace_literal_params, 10) \
o(1, 1, 1, replace_ifnz_literals, 11) \
o(1, 1, 1, jumpthread, 12) \
o(0, 1, 1, literal_jump_hardcode, 13) \
o(1, 1, 1, remove_redundant_ifnz, 14) \
o(0, 0, 1, remove_tail_call, 15) \
o(0, 1, 1, tac_arith_constfold, 16) \
o(0, 1, 1, tac_logic_constfold, 17) \
o(0, 1, 1, canonize_ifnz, 18) \
o(1, 1, 1, remove_useless_mov, 19) \
o(1, 1, 1, remove_fcall_result_cast, 20) \
o(0, 1, 1, rename_registers, 21) \
o(0, 1, 1, remap_registers, 22) \
o(1, 1, 1, prune_clobbers, 23) \
o(0, 1, 1, remove_stackops, 24)

static void setup_optimize(int level) {
	#define o(o1, o2, o3, name, _) optimize.push_back(level == 0 ? false : (level == 1 ? o1 : (level == 2 ? o2 : o3)));
	OPTIMIZE_FLAGS(o)
	#undef o
}

#define o(_, __, ___, name, i) \
inline static bool do_##name() { \
	return optimize[i]; \
}
OPTIMIZE_FLAGS(o)
#undef o

extern std::string arch;
extern int dumplevel;
extern std::vector<std::string> include_dirs;

struct arg_info {
	std::string input_name;
	std::string output_name = "a.out";
};

static void handle_m(std::string key, std::string val) {
	if (key == "arch") arch = val;
	else throw std::runtime_error("Unrecognized mode parameter");
}

static arg_info parseargs(int argc, char ** argv) {
	std::vector<std::string> arguments;
	for (int i = 1; i < argc; ++i) {
		arguments.emplace_back(argv[i]);
	}

	if (arguments.size() == 0 || arguments.back()[0] == '-') throw std::runtime_error("No file provided on arguments");
	if (!std::all_of(arguments.begin(), arguments.begin()+(arguments.size()-1), [](auto &s){return s[0] == '-';})) throw std::runtime_error("Invalid argument types");

	arg_info a{arguments.back()};
	arguments.pop_back();

	// Go through all of the arguments.
	int oplevel = 2;
	for (auto &arg : arguments) {
		// Switch on second
		switch (arg[1]) {
			case 'O':
				oplevel = std::stoi(arg.substr(2));
				break;
			case 'o':
				a.output_name = arg.substr(2);
				break;
			case 'V':
			case 'v':
				dumplevel = std::count(arg.begin(), arg.end(), 'V') + std::count(arg.begin(), arg.end(), 'v');
				break;
			case 'm':
				{
					std::string ent = arg.substr(2);
					if (ent.find('=') == std::string::npos) break;
					handle_m(ent.substr(0, ent.find('=')), ent.substr(ent.find('=')+1));
				}
				break;
			case 'I':
				{
					include_dirs.push_back(arg.substr(2));
				}
				break;
			case 'f':
				break;
			default:
				throw std::runtime_error("Unrecognized commandline flag: " + arg);
		}
	}

	auto pos = a.input_name.rfind('/');
	if (pos != std::string::npos) {
		include_dirs.push_back(a.input_name.substr(0, pos));
	}

	setup_optimize(oplevel);
	
	for (auto &arg : arguments) {
		if (arg[1] == 'f') {
#define o(_, __, ___, name, i) else if (arg.substr(2) == #name) optimize[i] = true; \
			else if (arg.substr(2) == "no_" #name) optimize[i] = false;

			if (arg.size() == 2) continue;
			OPTIMIZE_FLAGS(o)
			else throw std::runtime_error("Unrecognized commandline flag: " + arg);
#undef o
		}
	}

	return a;
}

#endif
