//
// Created by Alex on 2021/6/6.
//

#ifndef COMPILERFRONTCPP_PARSER_H
#define COMPILERFRONTCPP_PARSER_H

#include <Lexer.h>
#include <Expr.h>
#include <Env.h>

class Parser {
private:
    Lexer lex;    // lexical analyzer for this parser
    Token look;   // lookahead tagen
    Env *top = nullptr;
    int used = 0;         // storage used for declarations
public:
    Parser() {
        move();
    }
    Parser(Lexer lex) : lex(std::move(lex)) {}
    void move() { look = lex.scan(); }
    void error(const std::string &s) {
        std::cout << s;
    }

    void match(int t) {
        if (look.tag() == t) move();
        else error("syntax error");
    }

    void program() {
        StmtPtr s = block();
        int begin = s->newlabel();int after = s->newlabel();
        s->emitlabel(begin); s->gen(begin, after); s->emitlabel(after);
        s->codegen();
    }
    StmtPtr block() {
        Env env(top);
        match('{');
        decls();
        StmtPtr s = stmts();
        match('}');
        env.leave(top);
        return s;
    }

    void decls() {
        while( look.tag() == Token::BASIC ) {   // D -> type ID ;
            TypePtr p = type(); Token tok = look; match(Token::ID); match(';');
            IdPtr id = IdPtr(new Id(tok, p, used));
            top->put( tok, id );
            used = used + p->width();
        }
    }
    TypePtr type() {
        TypePtr p = look.type();
        match(Token::BASIC);
        if (look.tag() != '[') return p;
        else return dims(p);
    }
    TypePtr dims(TypePtr p) {
        match('[');
        Token tok = look;  match(Token::NUM); match(']');
        if (look.tag() == '[')
            p = dims(p);
        return Type::newArray(tok.number(), p);
    }
    StmtPtr stmts() {
        if (look.tag() == '}') { return Stmt::null(); }
        else {
            StmtPtr st = stmt();
            return SeqPtr(new Seq(st, stmts()));
        }

    }
    StmtPtr stmt() {
        ExprPtr x; StmtPtr s, s1, s2;
        StmtPtr savedStmt;
        switch (look.tag()) {
            case ';':
                move();
                return Stmt::null();
            case Token::IF:
                match(Token::IF); match('('); x = boolean(); match(')');
                s1 = stmt();
                if( look.tag() != Token::ELSE ) return IfPtr(new If(x, s1));
                match(Token::ELSE);
                s2 = stmt();
                return ElsePtr(new Else(x, s1, s2));

            case Token::WHILE: {
                WhilePtr whilenode = WhilePtr(new While());
                savedStmt = Stmt::Enclosing; Stmt::Enclosing = whilenode;
                match(Token::WHILE); match('('); x = boolean(); match(')');
                s1 = stmt();
                whilenode->init(x, s1);
                Stmt::Enclosing = savedStmt;  // reset Stmt.Enclosing
                return whilenode;
            }

            case Token::DO: {
                DoPtr donode = DoPtr(new Do());
                savedStmt = Stmt::Enclosing; Stmt::Enclosing = donode;
                match(Token::DO);
                s1 = stmt();
                match(Token::WHILE); match('('); x = boolean(); match(')'); match(';');
                donode->init(s1, x);
                Stmt::Enclosing = savedStmt;  // reset Stmt.Enclosing
                return donode;
            }
            case Token::BREAK:
                match(Token::BREAK); match(';');
                return BreakPtr(new Break());
            case '{':
                return block();

            default:
                return assign();
        }
    }

    StmtPtr assign() {
        StmtPtr stmt; Token t = look;
        match(Token::ID);
        IdPtr id = top->get(t);
        if (!id) error(t.toString() + " undeclared");
        if (look.tag() == '=') {
            move(); stmt = StmtPtr(new Set(id, boolean()));
        }
        else {
            AccessPtr x = offset(id);
            match('=');
            stmt = StmtPtr(new SetElem(x, boolean()));
        }
        match(';');
        return stmt;
    }
    ExprPtr boolean() {
        ExprPtr x = join();
        while (look.tag() == Token::OR) {
            Token tok = look; move(); x = ExprPtr(new Or(tok, x, join()));
        }
        return x;
    }
    ExprPtr join() {
        ExprPtr x = equality();
        while (look.tag() == Token::AND) {
            Token tok = look; move(); x = ExprPtr(new And(tok, x, equality()));
        }
        return x;
    }
    ExprPtr equality() {
        ExprPtr x = rel();
        while (look.tag() == Token::EQ || look.tag() == Token::NE) {
            Token tok = look; move(); x = ExprPtr(new Rel(tok, x, rel()));
        }
        return x;
    }
    ExprPtr rel() {
        ExprPtr x = expr();
        switch( look.tag() ) {
            case '<': case Token::LE: case Token::GE: case '>': {
                Token tok = look;  move();  return ExprPtr(new Rel(tok, x, expr()));
            }
            default:
                return x;
        }
    }
    ExprPtr expr() {
        ExprPtr x = term();
        while (look.tag() == '+' || look.tag() == '-') {
            Token tok = look; move(); x = ExprPtr(new Arith(tok, x, term()));
        }
        return x;
    }
    ExprPtr term() {
        ExprPtr x = unary();
        while (look.tag() == '*' || look.tag() == '/') {
            Token tok = look; move(); x = ExprPtr(new Arith(tok, x, unary()));
        }
        return x;
    }
    ExprPtr unary() {
        if (look.tag() == '-') {
            move(); return ExprPtr(new Unary(Token::minus, unary()));
        } else if(look.tag() == '!') {
            Token tok = look; move(); return ExprPtr(new Not(tok, unary()));
        }
        else return factor();
    }
    ExprPtr factor() {
        ExprPtr x;
        switch (look.tag()) {
            case '(':
                move(); x = boolean(); match(')');
                return x;
            case Token::NUM:
                x = ExprPtr(new Constant(look, Type::Int));    move(); return x;
            case Token::REAL:
                x = ExprPtr(new Constant(look, Type::Float));  move(); return x;
            case Token::TRUE:
                x = Constant::True;                   move(); return x;
            case Token::FALSE:
                x = Constant::False;                  move(); return x;
            default:
                error("syntax error");
                return x;
            case Token::ID: {
                //std::string s = look.toString();
                IdPtr id = top->get(look);
                if (!id) error(look.toString() + " undeclared");
                move();
                if (look.tag() != '[') return id;
                else return offset(id);
            }
        }
    }

    AccessPtr offset(IdPtr a) {  // I -> [E] | [E] I
        ExprPtr i;ExprPtr w; ExprPtr t1, t2;ExprPtr loc;  // inherit id
        TypePtr type = a->type;
        match('[');i = boolean(); match(']');
        type = type->of();
        w = ExprPtr(new Constant(type->width()));
        t1 = ExprPtr(new Arith(Token('*'), i, w));
        loc = t1;
        while (look.tag() == '[') {
            match('['); i = boolean(); match(']');
            type = type->of();
            t1 = ExprPtr(new Arith(Token('*'), i, w));
            t2 = ExprPtr(new Arith(Token('+'), loc, t1));
            loc = t2;
        }
        return AccessPtr(new Access(a, loc, type));
    }




};


#endif //COMPILERFRONTCPP_PARSER_H
