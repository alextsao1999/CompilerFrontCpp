//
// Created by Alex on 2021/6/6.
//

#ifndef COMPILERFRONTCPP_EXPR_H
#define COMPILERFRONTCPP_EXPR_H

#include <Node.h>
#include <memory>


extern llvm::LLVMContext TheContext;

static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
//static std::unique_ptr<KaleidoscopeJIT> TheJIT;

static void InitializeModule() {
  // Open a new module.
  // TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("My Module", TheContext);
  //TheModule->setDataLayout(TheJIT->getDataLayout());

  Builder = std::make_unique<llvm::IRBuilder<>>(TheContext);
}

struct Expr;
using ExprPtr = std::shared_ptr<Expr>;
struct Expr : public Node {
    Token op;
    TypePtr type;
    Expr(Token op, TypePtr type) : op(op), type(type) {}

    virtual ExprPtr gen() { return std::dynamic_pointer_cast<Expr>(shared_from_this()); }
    virtual ExprPtr reduce() { return std::dynamic_pointer_cast<Expr>(shared_from_this()); }

    virtual void jumping(int t, int f) { emitjumps(reduce()->toString(), t, f); }
    void emitjumps(const std::string &test, int t, int f) {
        if( t != 0 && f != 0 ) {
            emit("if " + test + " goto L" + std::to_string(t));
            emit("goto L" + std::to_string(f));
        }
        else if( t != 0 ) emit("if " + test + " goto L" + std::to_string(t));
        else if( f != 0 ) emit("iffalse " + test + " goto L" + std::to_string(f));
        else ; // nothing since both t and f fall through
    }

    std::string toString() override {
        return op.toString();
    }

};
struct Constant;
using ConstantPtr = std::shared_ptr<Constant>;
struct Constant : public Expr {
    using Expr::Expr;
    static ConstantPtr True;
    static ConstantPtr False;
    Constant(int i) : Expr(Token::num(i), Type::Int) {}
    //Constant(bool i) : Expr(i ? Token::True : Token::False, Type::Bool) {}
    void jumping(int t, int f) override {
      if ( this == True.get() && t != 0 ) emit("goto L" + std::to_string(t));
      else if ( this == False.get() && f != 0) emit("goto L" + std::to_string(f));
   }

   ValuePtr codegen() override {
       if (this == True.get()) return Builder->getTrue();
       if (this == False.get()) return Builder->getFalse();
       if (op.isReal()) return llvm::ConstantFP::get(TheContext, llvm::APFloat(op.real()));
       return Builder->getInt32(op.number());
   }
};
struct Id;
using IdPtr = std::shared_ptr<Id>;
struct Id : public Expr {
    int offset;
    ValuePtr value;
    Id(Token id, TypePtr p, int b) : Expr(id, p), offset(b) {
        value = Builder->CreateAlloca(p->type());
    }
    //	public String toString() {return "" + op.toString() + offset;}
   ValuePtr codegen() override {
       return value;
   }

};
struct Temp : public Expr {
    int number = 0;
    ValuePtr value;
    Temp(TypePtr p) : Expr(Token::temp(), p) {
        static int count = 0;
        number = ++count;
    }
    std::string toString() override {
        return "t" + std::to_string(number);
    }
};
struct Op : public Expr {
    using Expr::Expr;
    ExprPtr reduce() {
        ExprPtr x = gen();
        ExprPtr t = std::make_shared<Temp>(type);
        emit(t->toString() + " = " + x->toString());
        return t;
    }
};
struct Unary : public Op {
    ExprPtr expr;
    using Op::Op;
    Unary(Token tok, ExprPtr x) : Op(tok, Type::null()), expr(x) {
        type = Type::max(Type::Int, expr->type);
        if (type == Type::null()) error("type error");
    }
    ExprPtr gen() override {
        return std::make_shared<Unary>(op, expr->reduce()); 
    }
    std::string toString() override { return op.toString()+" "+expr->toString(); }
    virtual ValuePtr codegen() {
        if (op == '-') {
            //Builder->CreateSub()
            
            return Builder->CreateUnOp(llvm::Instruction::UnaryOps::FNeg, expr->codegen());
        }
        assert(!"not reachable");
        return nullptr;
    }


};
struct Arith : public Op {
    ExprPtr expr1, expr2;
    Arith(Token tok, ExprPtr x1, ExprPtr x2) : Op(tok, Type::null()), expr1(x1), expr2(x2) {
        type = Type::max(expr1->type, expr2->type);
        if (type == Type::null() ) error("type error");
    }
    ExprPtr gen() override {
        ExprPtr e1 = expr1->reduce();
        ExprPtr e2 = expr2->reduce();
        return std::make_shared<Arith>(op, e1, e2);
        //return std::make_shared<Arith>(op, expr1->reduce(), expr2->reduce());
    }
    std::string toString() override {
        return expr1->toString() + " " + op.toString() + " " + expr2->toString();
    }
    ValuePtr binaryOpInt(int op, ValuePtr L, ValuePtr R) {
        if (op == '+') {
            return Builder->CreateAdd(L, R, "addtemp");
        }
        if (op == '-') {
            return Builder->CreateSub(L, R, "subtemp");
        }
        if (op == '*') {
            return Builder->CreateMul(L, R, "multemp");
        }
        if (op == '/') {
            return Builder->CreateSDiv(L, R, "divtemp");
        }
        return nullptr;
    }
    ValuePtr binaryOpFloat(int op, ValuePtr L, ValuePtr R) {
        if (op == '+') {
            return Builder->CreateFAdd(L, R, "addtemp");
        }
        if (op == '-') {
            return Builder->CreateFSub(L, R, "subtemp");
        }
        if (op == '*') {
            return Builder->CreateFMul(L, R, "multemp");
        }
        if (op == '/') {
            return Builder->CreateFDiv(L, R, "divtemp");
        }
        return nullptr;
    }
    virtual ValuePtr codegen() {
        ValuePtr L = expr1->codegen();
        ValuePtr R = expr2->codegen();
        L = Type::cast(expr1->type, type, L);
        R = Type::cast(expr2->type, type, R);
        if (type == Type::Float) {
            return binaryOpFloat(op.tag(), L, R);
        }
        if (type == Type::Int) {
            return binaryOpInt(op.tag(), L, R);
        }
        
        assert(!"not reachable");
        return nullptr;
    }

};
struct Access;
using AccessPtr = std::shared_ptr<Access>;
struct Access : public Op {
    ExprPtr array;
    ExprPtr index;
    Access(ExprPtr a, ExprPtr i, TypePtr p) : Op(Token::word("[]", Token::INDEX), p) {
        array = a;
        index = i;
    }
    ExprPtr gen() override { return std::make_shared<Access>(array, index->reduce(), type); }
    void jumping(int t,int f) override { emitjumps(reduce()->toString(),t,f); }
    std::string toString() override {
        return array->toString() + " [ " + index->toString() + " ]";
    }
   ValuePtr codegen() override {
       ValuePtr v = Builder->CreateGEP(array->codegen(), index->codegen());
       return Builder->CreateLoad(v);
   }

};
struct Logical : public Expr {
    ExprPtr expr1, expr2;
    //using Expr::Expr;
    Logical(Token tok, ExprPtr x1, ExprPtr x2, TypePtr ty) : Expr(tok, Type::null()), expr1(x1), expr2(x2) {
        type = ty;
    }
    Logical(Token tok, ExprPtr x1, ExprPtr x2) : Expr(tok, Type::null()), expr1(x1), expr2(x2) {
        type = this->check(x1->type, x1->type);
        if (type == Type::null())
            error("type error");
    }
    virtual TypePtr check(TypePtr p1, TypePtr p2) {
        if (p1 == Type::Bool && p2 == Type::Bool) return Type::Bool;
        return Type::null();
    }
    ExprPtr gen() override {
        int f = newlabel();
        int a = newlabel();
        ExprPtr temp = std::make_shared<Temp>(type);
        jumping(0, f);
        emit(temp->toString() + " = true");
        emit("goto L" + std::to_string(a));
        emitlabel(f);emit(temp->toString() + " = false");
        emitlabel(a);
        return temp;
    }
    std::string toString() override {
        return expr1->toString() + " " + op.toString() + " " + expr2->toString();
    }
};
using Pred = llvm::CmpInst::Predicate;
struct And : public Logical {
    using Logical::Logical;
    void jumping(int t, int f) override {
      int label = f != 0 ? f : newlabel();
      expr1->jumping(0, label);
      expr2->jumping(t,f);
      if( f == 0 ) emitlabel(label);
   }

   ValuePtr codegen() override {
       ValuePtr ops[2] = {expr1->codegen(), expr2->codegen()};
       return Builder->CreateAnd(ops);
   }
};
struct Not : public Logical {
    Not(Token tok, ExprPtr x2) : Logical(tok, x2, x2) {}
    void jumping(int t, int f) override {
        expr2->jumping(f, t);
    }
    std::string toString() override {
        return op.toString() + " " + expr2->toString();
    }
   ValuePtr codegen() override {
       return Builder->CreateNot(expr1->codegen(), "nottemp");
   }

};

struct Or : public Logical {
    using Logical::Logical;
    void jumping(int t, int f) override {
        int label = t != 0 ? t : newlabel();
        expr1->jumping(label, 0);
        expr2->jumping(t,f);
        if( t == 0 ) emitlabel(label);
    }
    ValuePtr codegen() override {
        ValuePtr ops[2] = {expr1->codegen(), expr2->codegen()};
        return Builder->CreateOr(ops);
    }

};
struct Rel : public Logical {
    //using Logical::Logical;
    Rel(Token tok, ExprPtr x1, ExprPtr x2) : Logical(tok, x1, x2, Type::null()) {
        type = check(x1->type, x1->type);
        if (type == Type::null())
            error("type error");
    }

    TypePtr check(TypePtr p1, TypePtr p2) override {
        if(p1 == p2) return Type::Bool;
        return Type::null();
    }
    void jumping(int t, int f) override {
        ExprPtr a = expr1->reduce();
        ExprPtr b = expr2->reduce();
        std::string test = a->toString() + " " + op.toString() + " " + b->toString();
        emitjumps(test, t, f);
    }
    
    ValuePtr codegen() override {
        using InstOps = llvm::Instruction::OtherOps;
        using CmpOps = llvm::CmpInst::Predicate;
        ValuePtr L = expr1->codegen();
        ValuePtr R = expr2->codegen();
        L = Type::cast(expr1->type, type, L);
        R = Type::cast(expr2->type, type, R);
        InstOps instOp;
        CmpOps cmpOp;
        if (type == Type::Float) {
            instOp = InstOps::FCmp;
        }
        if (type == Type::Int) {
            instOp = InstOps::ICmp;
        }
        if (op == Token::EQ) {
            cmpOp = type == Type::Int ? CmpOps::ICMP_EQ : CmpOps::FCMP_OEQ;
        } else if(op == Token::GE) {
            cmpOp = type == Type::Int ? CmpOps::ICMP_SGE : CmpOps::FCMP_OGE;
        } else if(op == Token::LE) {
            cmpOp = type == Type::Int ? CmpOps::ICMP_SLE : CmpOps::FCMP_OLE;
        } else if(op == '>') {
            cmpOp = type == Type::Int ? CmpOps::ICMP_SGT : CmpOps::FCMP_OGT;
        } else if(op == '<') {
            cmpOp = type == Type::Int ? CmpOps::ICMP_SLT : CmpOps::FCMP_OLT;
        }
        return llvm::CmpInst::Create(instOp, cmpOp, L, R, "cmptemp");

     }
};
struct Stmt;
using StmtPtr = std::shared_ptr<Stmt>;
struct Stmt : public Node {
    static StmtPtr Enclosing;
    int after = 0;
    llvm::BasicBlock *leave = nullptr;
    virtual void gen(int b, int a) {}
    static StmtPtr null();
    static ValuePtr IfTrueValue(ExprPtr expr) {
       ValuePtr CondV = expr->codegen();
       if (!CondV) return nullptr;
       //Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(expr->type->type(), llvm::APInt(0)));
       if (expr->type == Type::Int) {
           ValuePtr zero = Builder->getIntN(expr->type->type()->getIntegerBitWidth(), 0);
           return Builder->CreateICmpNE(CondV, zero, "ifcond");
       } else if (expr->type == Type::Float) {
           ValuePtr zero = llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0));
           return Builder->CreateFCmpONE(CondV,zero , "ifcond");
       }
       return nullptr;
    }

};
struct Break;
using BreakPtr = std::shared_ptr<Break>;
struct Break : public Stmt {
    StmtPtr stmt;
    Break() {
        if( Stmt::Enclosing == nullptr ) error("unenclosed break");
        stmt = Stmt::Enclosing;
    }

    void gen(int b, int a) override {
        emit("goto L" + std::to_string(stmt->after));
    }
    ValuePtr codegen() override {
        //Builder->CreateBr()
        if (stmt->leave) {
            return Builder->CreateBr(stmt->leave);
        }
        return nullptr;
    }

};
struct Do;
using DoPtr = std::shared_ptr<Do>;
struct Do : public Stmt {

   ExprPtr expr; StmtPtr stmt;

   Do() { expr = nullptr; stmt = nullptr; }

   void init(StmtPtr s, ExprPtr x) {
      expr = x; stmt = s;
      if(expr->type != Type::Bool ) expr->error("boolean required in do");
   }

   void gen(int b, int a) override {
      after = a;
      int label = newlabel();   // label for expr
      stmt->gen(b,label);
      emitlabel(label);
      expr->jumping(b,0);
   }

   ValuePtr codegen() override {
       llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(TheContext, "cond");
       llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop");
       llvm::BasicBlock *LeaveBB = llvm::BasicBlock::Create(TheContext, "leave");
       llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
       TheFunction->getBasicBlockList().push_back(LoopBB);
       Builder->SetInsertPoint(LoopBB);
       ValuePtr LoopV = stmt->codegen();
       Builder->CreateBr(CondBB);
       LoopBB = Builder->GetInsertBlock();

       TheFunction->getBasicBlockList().push_back(CondBB);
       Builder->SetInsertPoint(CondBB);
       ValuePtr CondV = IfTrueValue(expr);
       //if (!CondV) return nullptr;
       Builder->CreateCondBr(CondV, LoopBB, LeaveBB);
       CondBB = Builder->GetInsertBlock();

       TheFunction->getBasicBlockList().push_back(LeaveBB);
       Builder->SetInsertPoint(LeaveBB);

       return nullptr;
   }
};

struct If;
using IfPtr = std::shared_ptr<If>;
struct If : public Stmt {
   ExprPtr expr; StmtPtr stmt;

   If(ExprPtr x, StmtPtr s) {
      expr = x;  stmt = s;
      if(expr->type != Type::Bool ) expr->error("boolean required in if");
   }

   void gen(int b, int a) override {
      int label = newlabel(); // label for the code for stmt
      expr->jumping(0, a);     // fall through on true, goto a on false
      emitlabel(label); if (stmt) stmt->gen(label, a);
   }

   ValuePtr codegen() override {
       ValuePtr CondV = IfTrueValue(expr);
       if (!CondV) return nullptr;
       //Builder->getIntN(expr->type->type()->getIntegerBitWidth(), 0);
       //Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(expr->type->type(), llvm::APInt(0)));
       CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "ifcond");
       llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
       llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
       llvm::BasicBlock *LeaveBB = llvm::BasicBlock::Create(TheContext, "leave", TheFunction);
       llvm::BranchInst *br = Builder->CreateCondBr(CondV, ThenBB, LeaveBB);
       
       TheFunction->getBasicBlockList().push_back(ThenBB);
       Builder->SetInsertPoint(ThenBB);
       ValuePtr ThenV = stmt->codegen();
       //if (!ThenV) return nullptr;
       ThenBB = Builder->GetInsertBlock();
       Builder->CreateBr(LeaveBB);

       TheFunction->getBasicBlockList().push_back(LeaveBB);
       Builder->SetInsertPoint(LeaveBB);
       //llvm::PHINode *PN = Builder->CreatePHI()
       //Builder->GetInsertBlock()
       return CondV;
   }
};

struct Else;
using ElsePtr = std::shared_ptr<Else>;
struct Else : public Stmt {

   ExprPtr expr; StmtPtr stmt1, stmt2;

   Else(ExprPtr x, StmtPtr s1, StmtPtr s2) {
      expr = x; stmt1 = s1; stmt2 = s2;
      if(expr->type != Type::Bool ) expr->error("boolean required in if");
   }
   void gen(int b, int a) override {
      int label1 = newlabel();   // label1 for stmt1
      int label2 = newlabel();   // label2 for stmt2
      expr->jumping(0,label2);    // fall through to stmt1 on true
      emitlabel(label1); if (stmt1) stmt1->gen(label1, a); emit("goto L" + std::to_string(a));
      emitlabel(label2); if (stmt2) stmt2->gen(label2, a);
   }
   ValuePtr codegen() override {
       ValuePtr CondV = expr->codegen();
       if (!CondV) return nullptr;
       //Builder->getIntN(expr->type->type()->getIntegerBitWidth(), 0);
       //Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(expr->type->type(), llvm::APInt(0)));
       CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "ifcond");
       llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
       llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
       llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else", TheFunction);
       llvm::BasicBlock *LeaveBB = llvm::BasicBlock::Create(TheContext, "leave", TheFunction);
       llvm::BranchInst *br = Builder->CreateCondBr(CondV, ThenBB, ElseBB);
       
       TheFunction->getBasicBlockList().push_back(ThenBB);
       Builder->SetInsertPoint(ThenBB);
       ValuePtr ThenV = stmt1->codegen();
       ThenBB = Builder->GetInsertBlock();
       Builder->CreateBr(LeaveBB);

       TheFunction->getBasicBlockList().push_back(ElseBB);
       Builder->SetInsertPoint(ElseBB);
       ValuePtr ElseV = stmt2->codegen();
       ElseV = Builder->GetInsertBlock();
       Builder->CreateBr(LeaveBB);

       TheFunction->getBasicBlockList().push_back(LeaveBB);
       Builder->SetInsertPoint(LeaveBB);
       return CondV;

   }
};

struct Seq;
using SeqPtr = std::shared_ptr<Seq>;
struct Seq : public Stmt {
   StmtPtr stmt1; StmtPtr stmt2;

   Seq(StmtPtr s1, StmtPtr s2) { stmt1 = s1; stmt2 = s2; }

   void gen(int b, int a) override {
      if ( stmt1 == nullptr ) stmt2->gen(b, a);
      else if ( stmt2 == nullptr ) stmt1->gen(b, a);
      else {
         int label = newlabel();
         stmt1->gen(b,label);
         emitlabel(label);
         stmt2->gen(label,a);
      }
   }
   ValuePtr codegen() override {
       stmt1->codegen();
       return stmt2->codegen();
   }
};
struct Set;
using SetPtr = std::shared_ptr<Set>;
struct Set : public Stmt {
   ExprPtr id; ExprPtr expr;
   Set(ExprPtr i, ExprPtr x) {
      id = i; expr = x;
      if (check(id->type, expr->type) == Type::null() )
          error("type error");
   }

   TypePtr check(TypePtr p1, TypePtr p2) {
      if (Type::numeric(p1) && Type::numeric(p2) ) return p2;
      else if (p1 == Type::Bool && p2 == Type::Bool ) return p2;
      else return Type::null();
   }

   void gen(int b, int a) override {
      emit( id->toString() + " = " + expr->gen()->toString() );
   }
    ValuePtr codegen() {
        return Builder->CreateStore(expr->codegen(), id->codegen());
    }

};
struct SetElem;
using SetElemPtr = std::shared_ptr<SetElem>;
struct SetElem : public Stmt {

   ExprPtr array; ExprPtr index; ExprPtr expr;

   SetElem(AccessPtr x, ExprPtr y) {
      array = x->array; index = x->index; expr = y;
      if (check(x->type, expr->type) == Type::null() ) error("type error");
   }

   TypePtr check(TypePtr p1, TypePtr p2) {
      if ( p1->isArray() || p2->isArray() ) return Type::null();
      else if ( p1 == p2 ) return p2;
      else if (Type::numeric(p1) && Type::numeric(p2) ) return p2;
      else return Type::null();
   }

   void gen(int b, int a) override {
      std::string s1 = index->reduce()->toString();
      std::string s2 = expr->reduce()->toString();
      emit(array->toString() + " [ " + s1 + " ] = " + s2);
   }
   ValuePtr codegen() override {
       ValuePtr Base = array->codegen();
       ValuePtr Offset = Builder->CreateGEP(Base, index->codegen());
       ValuePtr RHS = expr->codegen();

       return Builder->CreateStore(RHS, Offset);
   }
};
struct While;
using WhilePtr = std::shared_ptr<While>;
struct While : public Stmt {

   ExprPtr expr; StmtPtr stmt;

   While() { expr = nullptr; stmt = nullptr; }

   void init(ExprPtr x, StmtPtr s) {
      expr = x;  stmt = s;
      if(expr->type != Type::Bool ) expr->error("boolean required in while");
   }
   void gen(int b, int a) override {
       after = a;                // save label a
       expr->jumping(0, a);
       int label = newlabel();   // label for stmt
       emitlabel(label);
       stmt->gen(label, b);
       emit("goto L" + std::to_string(b));
   }

   ValuePtr codegen() override {
       llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(TheContext, "cond");
       llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop");
       llvm::BasicBlock *LeaveBB = llvm::BasicBlock::Create(TheContext, "leave");
       llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

       TheFunction->getBasicBlockList().push_back(CondBB);
       Builder->SetInsertPoint(CondBB);
       ValuePtr CondV = IfTrueValue(expr);
       //if (!CondV) return nullptr;
       Builder->CreateCondBr(CondV, LoopBB, LeaveBB);
       CondBB = Builder->GetInsertBlock();

       TheFunction->getBasicBlockList().push_back(LoopBB);
       Builder->SetInsertPoint(LoopBB);
       ValuePtr LoopV = stmt->codegen();
       Builder->CreateBr(CondBB);
       LoopBB = Builder->GetInsertBlock();

       TheFunction->getBasicBlockList().push_back(LeaveBB);
       Builder->SetInsertPoint(LeaveBB);

       return CondV;
   }
};


#endif //COMPILERFRONTCPP_EXPR_H
