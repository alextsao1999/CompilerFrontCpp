//
// Created by Alex on 2021/6/5.
//

#ifndef COMPILERFRONTCPP_NODE_H
#define COMPILERFRONTCPP_NODE_H

#include <map>
#include <string>
#include <Lexer.h>
#include <Token.h>
#include <iostream>
#include <memory>
struct Node : public std::enable_shared_from_this<Node> {
    int lexline = 0;
    Node() {
        lexline = Lexer::line;
    }
    virtual ~Node() {}
    void error(const std::string& s) {
        std::cout << s << std::endl;
    }
    int newlabel() {
        static int labels = 0;
        return ++labels;
    }
    void emitlabel(int i) {
        std::cout << "L" << i << ":";
    }
    void emit(const std::string &str) {
        std::cout << "\t" << str << std::endl;
    }
    virtual std::string toString() { return ""; }

};
using NodePtr = std::shared_ptr<Node>;

#endif //COMPILERFRONTCPP_NODE_H
