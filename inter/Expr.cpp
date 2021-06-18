//
// Created by Alex on 2021/6/6.
//

#include "Expr.h"

Token Token::True{"true", Token::TRUE};
Token Token::False{"false", Token::FALSE};
Token Token::minus{"minus", Token::MINUS};

#define GetBasic(LEXEME, WIDTH) \
    ([]() -> Type *{ \
        static Type basic(#LEXEME, WIDTH); \
        return &basic; \
    })()

Type* Type::Int = GetBasic(int, 4);
Type* Type::Float = GetBasic(float, 8);
Type* Type::Char = GetBasic(char, 1);
Type* Type::Bool = GetBasic(bool, 1);

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
