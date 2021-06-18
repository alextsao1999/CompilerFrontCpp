//
// Created by Alex on 2021/6/5.
//

#ifndef COMPILERFRONTCPP_TOKEN_H
#define COMPILERFRONTCPP_TOKEN_H
#include "assert.h"
#include <string_view>
#include <string>
#include <vector>
#include <memory>
class Type;
using TypePtr = Type *;

class Token {
    int tag_ = 0;
    std::string lexeme_ = "";
    union {
        int number_;
        float real_;
        Type *type_;
    };
public:
    static Token True;
    static Token False;
    static Token minus;
    //static Token type(const std::string &s, int width) { return Token(s, BASIC, width); }
    static Token word(const std::string &s, int tag) { return Token(s, tag); }
    static Token real(float value) { return Token(REAL, value); }
    static Token num(int value) { return Token(NUM, value); }
    static Token temp() { return Token("t", TEMP); }
    enum Tag {
        AND   = 256,  BASIC = 257,  BREAK = 258,  DO   = 259, ELSE  = 260,
        EQ    = 261,  FALSE = 262,  GE    = 263,  ID   = 264, IF    = 265,
        INDEX = 266,  LE    = 267,  MINUS = 268,  NE   = 269, NUM   = 270,
        OR    = 271,  REAL  = 272,  TEMP  = 273,  TRUE = 274, WHILE = 275,
    };
    Token() = default;
    Token(int tag) : tag_(tag) {}
    Token(const std::string &s, int tag) : tag_(tag), lexeme_(s) {}
    Token(const std::string &s, int tag, Type *ty) : tag_(tag), lexeme_(s), type_(ty) {}
    //Token(std::string s, int tag, int width) : tag_(tag), width_(width), lexeme_(std::move(s)) {}
    Token(int tag, int value) : number_(value), tag_(tag) {}
    Token(int tag, float value) : real_(value), tag_(tag) {}
    int tag() const { return tag_; }
    bool isNum() const { return tag_ == NUM; }
    bool isReal() const { return tag_ == REAL; }
    bool isType() const { return tag_ == BASIC; }
    const std::string &lexeme() const { return  lexeme_; }
    Type *type() { assert(tag_ == BASIC || tag_ == INDEX);return type_; }
    int number() { assert(tag_ == NUM);return number_; }
    int real() { assert(tag_ == REAL);return real_; }
    bool operator==(const Token &rhs) {
        return tag_ == rhs.tag_ 
            && lexeme_ == rhs.lexeme_
            && type_ == rhs.type_;
            ;
    }
    bool operator!=(const Token &rhs) {
        return !(*this == rhs);
    }

    std::string toString() {
        if (tag_ < AND) {
            //return std::string((const char *)&tag_, 1);
            return std::string(1, tag_);
        }
        switch (tag_) {
            case REAL:
                return std::to_string(real_);
            case NUM:
                return std::to_string(number_);
            default:
                return lexeme_;
        }

    }
};
class Type : public Token {
    int width_ = 0;
    bool isArray_ = false;
    Type *of_ = null();
    int size_ = 0;
public:
    static Type *Char;
    static Type *Float;
    static Type *Int;
    static Type *Bool;
    static Type *null() {
        return nullptr;
    }
    static Type *max(Type *p1, Type *p2) {
        if ( ! numeric(p1) || ! numeric(p2) ) return null();
        else if ( p1 == Float || p2 == Float ) return Float;
        else if ( p1 == Int   || p2 == Int   ) return Int;
        else return Char;
    }
    static bool numeric(Type *p) {
        return p == Char || p == Int || p == Float;
    }
    static Type *newArray(int sz, Type *p) {
        static std::vector<std::unique_ptr<Type>> container;
        Type *ty = new Type(sz, p);
        container.emplace_back(ty);
        return ty;
    }
    Type() : Token() {}
    Type(std::string s, int width) : Token(s, Token::BASIC, this), width_(width) {}
    Type(int sz, Type *p) : Token("[]", Token::INDEX, this), of_(p), width_(sz * p->width_), size_(sz), isArray_(true) {}
    Type *of() {
        return of_;
    }
    bool isArray() {
        return isArray_;
    }
    int width() { return width_; }
};

#endif //COMPILERFRONTCPP_TOKEN_H
