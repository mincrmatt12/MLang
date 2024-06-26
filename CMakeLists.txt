cmake_minimum_required(VERSION 3.9)
project(mlang)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake CACHE INTERNAL "" FORCE)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
	add_compile_options(-O3)
else()
	add_compile_options(-g)
endif()

#add_compile_options(-Wall -Wno-unused-function)

find_package(RE2C REQUIRED)
find_package(BISON REQUIRED)

RE2C_TARGET(NAME mlang_re2c INPUT ${CMAKE_SOURCE_DIR}/src/parser.y OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/parser.y.re)
BISON_TARGET(mlang_yacc ${CMAKE_CURRENT_BINARY_DIR}/parser.y.re ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/parser.h)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

message(STATUS ${BISON_mlang_yacc_OUTPUTS})
add_executable(mlc src/args.cpp src/main.cpp src/ast_optimize.cpp
	           src/compiler.cpp src/tac_optimize.cpp src/preprocess.cpp 
			   src/codegen_64.cpp src/codegen_cgotos.cpp src/flow.cpp ${BISON_mlang_yacc_OUTPUTS})

# Install
install(TARGETS mlc RUNTIME DESTINATION bin)
