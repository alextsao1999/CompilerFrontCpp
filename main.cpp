#include <iostream>
#include <Parser.h>
#include "llvm/IR/Module.h"
#include <llvm/Support/SourceMgr.h>
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/Host.h"
#include "llvm/IR/Verifier.h"
int main(int argc, char **argv) {
    struct Options {
        enum {
            None,
            IR,
            ASM,
        } Emit = None;
    } Options;

    for (int index = 1; index < argc; index++) {
        if (strcmp(argv[index], "-ll") == 0 || strcmp(argv[index], "-emit-llvm") == 0) {
            Options.Emit = Options::IR;
            index += 1;
        } else if (strcmp(argv[index], "-s") == 0 || strcmp(argv[index], "-emit-asm") == 0) {
            Options.Emit = Options::ASM;
            index += 1;
        } else if (strcmp(argv[index], "-h") == 0 || strcmp(argv[index], "--help") == 0) {
            std::cout << "Usage: [options]" << std::endl;
            std::cout << "Options:" << std::endl;
            std::cout << "  -ll, -emit-llvm\tEmit llvm ir" << std::endl;
            std::cout << "  -s, -emit-asm\tEmit riscv64 asm" << std::endl;
            std::cout << "  -h, --help" << std::endl;
            return 0;
        }
    }

    InitializeModule();
    Parser parser;
    auto s = parser.program();

    auto EmitASM = [] {
        ///// 生成汇编代码 /////
        using namespace llvm;
        if (verifyModule(*TheModule, &llvm::errs())) {
            std::cerr << "Error: LLVM IR is invalid!\n";
            return 1;
        }

        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmPrinters();
        InitializeAllAsmParsers();

        std::string errors;
        if (const Target *target = TargetRegistry::lookupTarget("riscv64", errors)) {
            TargetOptions targetOptions;
            TargetMachine *targetMachine = target->createTargetMachine("riscv64", "", "", targetOptions, {});
            TheModule->setDataLayout(targetMachine->createDataLayout());
            legacy::PassManager passManager;
            targetMachine->addPassesToEmitFile(passManager, outs(), nullptr, CGFT_AssemblyFile);
            passManager.run(*TheModule);
        } else {
            std::cerr << errors;
            return 1;
        }
        return 0;
    };

    switch (Options.Emit) {
        case Options::None:
            ///// 生成伪代码 /////
        {
            int begin = s->newlabel();int after = s->newlabel();
            s->emitlabel(begin); s->gen(begin, after); s->emitlabel(after);
        }
            break;
        case Options::IR:
            ///// 生成LLVM IR /////
            s->codegen();
            Builder->CreateRet(Builder->getInt32(0));
            llvm::outs() << *TheModule << "\n";
            break;
        case Options::ASM:
            return EmitASM();
    }

    return 0;
}

