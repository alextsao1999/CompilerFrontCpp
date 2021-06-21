//
// Created by Alex on 2021/6/6.
//

#include "Expr.h"

llvm::LLVMContext TheContext;

Token Token::True{"true", Token::TRUE};
Token Token::False{"false", Token::FALSE};
Token Token::minus{"minus", Token::MINUS};

#define GetBasic(LEXEME, WIDTH, TY) \
    ([]() -> Type *{ \
        static Type basic(#LEXEME, WIDTH, TY); \
        return &basic; \
    })()

Type* Type::Int = GetBasic(int, 4, llvm::Type::getInt32Ty(TheContext));
Type* Type::Float = GetBasic(float, 8, llvm::Type::getFloatTy(TheContext));
Type* Type::Char = GetBasic(char, 1, llvm::Type::getInt1Ty(TheContext));
Type* Type::Bool = GetBasic(bool, 1, llvm::Type::getInt1Ty(TheContext));

ConstantPtr Constant::True = []() {
    static ConstantPtr con = ConstantPtr(new Constant(Token::True, Type::Bool));
    return con;
}();

ConstantPtr Constant::False = []() {
    static ConstantPtr con = ConstantPtr(new Constant(Token::False, Type::Bool));
    return con;
}();

StmtPtr Stmt::Enclosing = nullptr;

StmtPtr Stmt::null() {
    return StmtPtr();
}
