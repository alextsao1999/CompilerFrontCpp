//
// Created by Alex on 2021/6/5.
//

#ifndef COMPILERFRONTCPP_LEXER_H
#define COMPILERFRONTCPP_LEXER_H


#include <map>
#include <iostream>
#include <cmath>
#include "Token.h"
class Lexer {
public:
    static int line;
    char peek = ' ';
    std::map<std::string, Token> words;
    void reverse(const Token &token) {
        words.emplace(token.lexeme(), token);
    }

    Lexer() {
        reverse(Token::word("if", Token::IF));
        reverse(Token::word("else", Token::ELSE));
        reverse(Token::word("while", Token::WHILE));
        reverse(Token::word("do", Token::DO));
        reverse(Token::word("break", Token::BREAK));
        reverse(Token::True);
        reverse(Token::False);
        reverse(*(Token *) Type::Int);
        reverse(*(Token *) Type::Char);
        reverse(*(Token *) Type::Bool);
        reverse(*(Token *) Type::Float);
    }
    void readch() {
        //peek = (char) std::cin.get();
        peek = getchar();
    }
    bool readch(char c) {
        readch();
        if (peek != c) return false;
        peek = ' ';
        return true;
    }
    Token scan() {
        for(;;readch()) {
            if (peek == ' ' || peek == '\t') continue;
            else if(peek == '\n') line++;
            else break;
        }
        switch (peek) {
            case '&':
                if( readch('&') ) return Token("&&", Token::AND);  else return Token('&');
            case '|':
                if( readch('|') ) return Token("||", Token::OR);   else return Token('|');
            case '=':
                if( readch('=') ) return Token("==", Token::EQ);   else return Token('=');
            case '!':
                if( readch('=') ) return Token("!=", Token::NE);   else return Token('!');
            case '<':
                if( readch('=') ) return Token("<=", Token::LE);   else return Token('<');
            case '>':
                if( readch('=') ) return Token(">=", Token::GE);   else return Token('>');
        }
        if (isdigit(peek)) {
            int v = 0;
            do {
                v = 10 * v + (peek - '0');
                readch();
            } while( isdigit(peek) );
            if(peek != '.') return Token::num(v);
            float x = v; float d = 10;
            for(;;) {
                readch();
                if(!isdigit(peek)) break;
                x = x + (peek - '0') / d;
                d = d * 10;
            }
            return Token::real(x);
        }
        if (isalpha(peek)) {
            std::string b;
            do {
                b += peek;
                readch();
            } while (isalnum(peek));
            if (words.count(b)) {
                return words.find(b)->second;
            }
            Token w = Token::word(b, Token::ID);
            words.emplace(b, w);
            return w;
        }
        Token tok(peek);
        peek = ' ';
        return tok;
    }

};


#endif //COMPILERFRONTCPP_LEXER_H
