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
int main() {
    InitializeModule();
    Parser parser;
    auto s = parser.program();

    ///// 生成伪代码 /////
    int begin = s->newlabel();int after = s->newlabel();
    s->emitlabel(begin); s->gen(begin, after); s->emitlabel(after);

    ///// 生成LLVM IR /////
    s->codegen();
    Builder->CreateRet(Builder->getInt32(0));
    llvm::outs() << *TheModule << "\n";

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
    const Target *target = TargetRegistry::lookupTarget("riscv64", errors);

    TargetOptions targetOptions;
    TargetMachine *targetMachine = target->createTargetMachine("riscv64", "", "", targetOptions, {});
    TheModule->setDataLayout(targetMachine->createDataLayout());

    legacy::PassManager passManager;
    targetMachine->addPassesToEmitFile(passManager, outs(), nullptr, CGFT_AssemblyFile);
    passManager.run(*TheModule);

    return 0;
}

