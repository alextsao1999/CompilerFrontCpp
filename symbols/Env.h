//
// Created by Alex on 2021/6/6.
//

#ifndef COMPILERFRONTCPP_ENV_H
#define COMPILERFRONTCPP_ENV_H

#include <map>
#include <string>
#include <Expr.h>

class Env {
    std::map<std::string, IdPtr> table_;
    Env *outer_;
public:
    Env(Env *&outer) : outer_(outer) {
        outer = this;
    }
    void leave(Env *&outer) {
        outer = outer_;
    }
    void put(Token t, IdPtr id) {
        table_[t.lexeme()] = id;
    }

    IdPtr get(Token w) {
        if (table_.count(w.lexeme())) {
            return table_[w.lexeme()];
        }
        return outer_ ? outer_->get(w) : nullptr;
    }
    inline Env *outer() { return outer_; }
};


#endif //COMPILERFRONTCPP_ENV_H
