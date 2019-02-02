#include "preprocess.h"
#include <fstream>
#include <iostream>
#include <set>
#include <cstdlib>

std::set<std::string> included_files{};

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
					if (pos == std::string::npos) throw std::runtime_error("invalid include: no filename");
					else {
						char * resolved = realpath(line.substr(pos + 1).c_str(), nullptr);
						std::string resolved_path{resolved};
						free(resolved);

						auto [_, b] = included_files.insert(resolved_path);
						if (b) {
							std::ifstream f(resolved_path);
							out += preprocess_string(f, line.substr(pos + 1));
							out += "#file " + filename + '\n';
							out += "#line " + std::to_string(lineno + 1) + '\n';
							continue;
						} 
					}
				}
				else if (command == "line" || command == "file") {}
			}
		}
		out += line;
		out += '\n';
		++lineno;
	}

	return out;
}
