cmake_minimum_required(VERSION 3.17)
project(CompilerFrontCpp)

set(CMAKE_CXX_STANDARD 17)

# LLVM
find_package(LLVM REQUIRED CONFIG)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

foreach(dir inter;lexer;symbols;parser)
    include_directories(${dir})
    aux_source_directory(${dir} SOURCES)
endforeach()

add_executable(CompilerFrontCpp main.cpp ${SOURCES})
