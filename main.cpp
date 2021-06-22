#include <iostream>
#include <Parser.h>

int main() {
    InitializeModule();
    Parser parser;
    parser.program();
    //std::cout << "Hello, World!" << std::endl;
    llvm::outs() << *TheModule;
    return 0;
}

