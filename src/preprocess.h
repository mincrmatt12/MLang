// Preprocessor -- similar to C++
// 
// current commands:
// #include "abc.mlang" -- verbatim includes a file. only ever includes a file once

#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <string>

std::string preprocess_string(std::istream& in, std::string filename); 

#endif
