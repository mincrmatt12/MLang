#include "preprocess.h"
#include "args.h"
#include <fstream>
#include <sys/stat.h>
#include <iostream>
#include <set>
#include <cstdlib>

std::set<std::string> included_files{};

char * resolve_path(std::string in_path) {
	struct stat buffer;
	if (stat(in_path.c_str(), &buffer) == 0) return realpath(in_path.c_str(), nullptr);

	for (const auto& e : include_dirs) {
		if (e[e.size()-1] == '/') {
			if (stat((e + in_path).c_str(), &buffer) == 0)       return realpath((e + in_path).c_str(), nullptr);
		}
		else {
			if (stat((e + '/' + in_path).c_str(), &buffer) == 0) return realpath((e + '/' + in_path).c_str(), nullptr);				
		}
	}

	return nullptr;
}

std::string preprocess_string(std::istream &in, std::string filename) {
	std::string out{};
	out += "#file " + filename + '\n';
	int lineno = 1;
	for (std::string line; std::getline(in, line); ) {
		if (line.size() > 0) {
			if (line[0] == '#') {
				// this is a preprocessor command
				
				std::size_t pos = line.find(' ');
				std::string command = line.substr(1, pos-1);

				if (command == "include") {
					if (pos == std::string::npos) throw std::runtime_error("line " + std::to_string(lineno) + ": invalid include: no filename");
					else {
						std::string fname = line.substr(pos + 1);
						char * resolved = resolve_path(fname);
						if (resolved == nullptr) {
							throw std::runtime_error("line " + std::to_string(lineno) + ": invalid include: file " + fname + " not found");
						}
						std::string resolved_path{resolved};
						free(resolved);

						auto [_, b] = included_files.insert(resolved_path);
						if (b) {
							std::ifstream f(resolved_path);
							out += preprocess_string(f, fname);
							out += "#file " + filename + '\n';
							out += "#line " + std::to_string(lineno + 1) + '\n';
						} 
						continue;
					}
				}
				else if (command == "line" || command == "file") {}
				else {
					throw std::runtime_error("line " + std::to_string(lineno) + ": invalid preprocessor command: " + command);
				}
			}
		}
		out += line;
		out += '\n';
		++lineno;
	}

	return out;
}
