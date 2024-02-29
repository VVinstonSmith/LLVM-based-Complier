// #define ENABLE_JIT
#ifdef ENABLE_JIT
    #include "../include/KaleidoscopeJIT.h"
    #include "llvm/ADT/APFloat.h"
    #include "llvm/ADT/STLExtras.h"
    #include "llvm/IR/BasicBlock.h"
    #include "llvm/IR/Constants.h"
    #include "llvm/IR/DerivedTypes.h"
    #include "llvm/IR/Function.h"
    #include "llvm/IR/IRBuilder.h"
    #include "llvm/IR/Instructions.h"
    #include "llvm/IR/LLVMContext.h"
    #include "llvm/IR/Module.h"
    #include "llvm/IR/PassManager.h"
    #include "llvm/IR/Type.h"
    #include "llvm/IR/Verifier.h"
    #include "llvm/Passes/PassBuilder.h"
    #include "llvm/Passes/StandardInstrumentations.h"
    #include "llvm/Support/TargetSelect.h"
    #include "llvm/Target/TargetMachine.h"
    #include "llvm/Transforms/InstCombine/InstCombine.h"
    #include "llvm/Transforms/Scalar.h"
    #include "llvm/Transforms/Scalar/GVN.h"
    #include "llvm/Transforms/Scalar/Reassociate.h"
    #include "llvm/Transforms/Scalar/SimplifyCFG.h"
    #include "llvm/Transforms/Utils.h"
    #include <algorithm>
    #include <cassert>
    #include <cctype>
    #include <cstdint>
    #include <cstdio>
    #include <cstdlib>
    #include <map>
    #include <memory>
    #include <string>
    #include <utility>
    #include <vector>
    using namespace llvm;
    using namespace llvm::orc;
#else
    #include "llvm/ADT/APFloat.h"
    #include "llvm/ADT/STLExtras.h"
    #include "llvm/IR/BasicBlock.h"
    #include "llvm/IR/Constants.h"
    #include "llvm/IR/DerivedTypes.h"
    #include "llvm/IR/Function.h"
    #include "llvm/IR/IRBuilder.h"
    #include "llvm/IR/Instructions.h"
    #include "llvm/IR/LLVMContext.h"
    #include "llvm/IR/LegacyPassManager.h"
    #include "llvm/IR/Module.h"
    #include "llvm/IR/Type.h"
    #include "llvm/IR/Verifier.h"
    #include "llvm/MC/TargetRegistry.h"
    #include "llvm/Support/FileSystem.h"
    #include "llvm/Support/TargetSelect.h"
    #include "llvm/Support/raw_ostream.h"
    #include "llvm/Target/TargetMachine.h"
    #include "llvm/Target/TargetOptions.h"
    #include "llvm/TargetParser/Host.h"
    #include <algorithm>
    #include <cassert>
    #include <cctype>
    #include <cstdio>
    #include <cstdlib>
    #include <map>
    #include <memory>
    #include <string>
    #include <system_error>
    #include <utility>
    #include <vector>
    using namespace llvm;
    using namespace llvm::sys;
#endif

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

enum Token{ // for unkown token, return [0-255]
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,

    // control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,

    // operators
    tok_binary = -11,
    tok_unary = -12,

    // var definition
    tok_var = -13
};

static std::string IdentifierStr; // filled in if tok_identifier
static double NumVal;             // filled in if tok_number

static int gettok(){
    static int LastChar = ' ';

    // skip whitespace
    while(isspace(LastChar))
        LastChar = getchar();

    // IdentifierStr is tok_def, tok_external or tok_identifier
    if(isalpha(LastChar)){ // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while(isalnum(LastChar = getchar())) // until LastChar is not litter or number
            IdentifierStr += LastChar;

        if(IdentifierStr == "def")
            return tok_def;
        if(IdentifierStr == "extern")
            return tok_extern;
        if(IdentifierStr == "if")
            return tok_if;
        if(IdentifierStr == "then")
            return tok_then;
        if(IdentifierStr == "else")
            return tok_else;
        if(IdentifierStr == "for")
            return tok_for;
        if(IdentifierStr == "in")
            return tok_in;
        if(IdentifierStr == "binary")
            return tok_binary;
        if(IdentifierStr == "unary")
            return tok_unary;
        if(IdentifierStr == "var")
            return tok_var;
        return tok_identifier;
    }

    // number: [0-9.]
    if(isdigit(LastChar) || LastChar == '.'){
        std::string NumStr;
        do{
            NumStr += LastChar;
            LastChar = getchar();
        } while(isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    // is a comment
    if(LastChar == '#'){
        do{
            LastChar = getchar();
        } while(LastChar!=EOF && LastChar!='\n' && LastChar!='\r');
        if(LastChar != EOF)
            return gettok(); // get the first tok of the next line
    }

    // if end of the file
    if(LastChar==EOF)
        return tok_eof;

    // if not identifier, not number, not comment, then return the character
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual Value* codegen() = 0;
};

// Expression class for literals like "1.0".
class NumberExprAST : public ExprAST {
    double Val;
public:
    NumberExprAST(double Val) : Val(Val) {}
    Value* codegen() override;
};

// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value* codegen() override;
    const std::string &getName() const { return Name; }
};

//UnaryExprAST - Expression class for a unary operator
class UnaryExprAST : public ExprAST {
    char Opcode;
    std::unique_ptr<ExprAST> Operand;
public:
    UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
        : Opcode(Opcode), Operand(std::move(Operand)) {}
    Value* codegen() override;
};

// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS) 
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value* codegen() override;
};

// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    Value* codegen() override;
};

// IfExprAST - Expression class for if/then/else
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
public:
    IfExprAST(std::unique_ptr<ExprAST> Cond,
        std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else)
        : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    Value* codegen() override;
};

// ForExprAST - Expression class for for/in
class ForExprAST : public ExprAST {
    std::string VarName;
    std::unique_ptr<ExprAST> Start, End, Step, Body;
public:
    ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
        std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
        std::unique_ptr<ExprAST> Body)
        : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}
    Value* codegen() override;
};

// varExprAST - Expression class for var/in
class VarExprAST : public ExprAST {
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
    std::unique_ptr<ExprAST> Body;
public:
    VarExprAST(std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
        std::unique_ptr<ExprAST> Body)
        : VarNames(std::move(VarNames)), Body(std::move(Body)) {}
    Value* codegen() override;
};

// PrototypeAST - This class represents the "prototype" for a function
// which captures its name, and its argument names (thus implicitly the number
// of arguments the function takes), as well as if it is an operator.
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
    bool IsOperator; // if it is an operator
    int Precedence; // the precedence of the operator
public:
    PrototypeAST(const std::string &Name,  std::vector<std::string> Args,
        bool IsOperator = false, int Prec = 0)
        : Name(Name), Args(std::move(Args)), 
        IsOperator(IsOperator), Precedence(Prec) {}
    
    Function* codegen();
    const std::string &getName() const { return Name; }

    bool isUnaryOp() const { return IsOperator && Args.size()==1; }
    bool isBinaryOp() const { return IsOperator && Args.size()==2; }
    char getOperatorName() const {
        assert(isUnaryOp() || isBinaryOp());
        return Name[Name.size()-1];
    }
    int getBinaryPrecedence() const { return Precedence; }
};

// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;
public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    Function* codegen();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence(){
    if(!isascii(CurTok))
        return -1;
    
    int TokPrec = BinopPrecedence[CurTok];
    if(TokPrec <= 0)
        return -1;
    return TokPrec;
}

// helper functions for error handling
std::unique_ptr<ExprAST> LogError(const char *Str){
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str){
    LogError(Str);
    return nullptr;
}

// expression refers to "LHS Op RHS"
static std::unique_ptr<ExprAST> ParseExpression();

// number_expr
// ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr(){
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken();
    return std::move(Result);
}

// paren_expr
// ::= '(' expresson ')'
static std::unique_ptr<ExprAST> ParseParenExpr(){
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if(!V)
        return nullptr;
    if(CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // eat ).
    return V;
}

// identifier_expr
// ::= identifier
// ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;
    getNextToken(); // eat identifier.
    if(CurTok != '(') // simple variable ref.
        return std::make_unique<VariableExprAST>(IdName); // return a variable
    
    getNextToken(); //eat '('
    std::vector<std::unique_ptr<ExprAST>> Args;
    if(CurTok != ')'){ // has expression
        while(true){
            if(auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;
            
            if(CurTok == ')') 
                break;
            if(CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken(); // eat ','
        }
    }
    getNextToken(); // eat ')'
    return std::make_unique<CallExprAST>(IdName, std::move(Args)); // return a function call
}

// if_expr
// ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr(){
    getNextToken(); // eat if.

    // parse condition
    auto Cond = ParseExpression();
    if(!Cond)
        return nullptr;

    if(CurTok != tok_then)
        return LogError("expected then");
    getNextToken();

    // parse then_expr
    auto Then = ParseExpression();
    if(!Then)
        return nullptr;
    
    if(CurTok != tok_else)
        return LogError("expected else");
    getNextToken();

    // parse else_expr
    auto Else = ParseExpression();
    if(!Else)
        return nullptr;

    return std::make_unique<IfExprAST>(std::move(Cond), 
        std::move(Then), std::move(Else));
}

// for_expr
// ::= 'for' id '=' Start ',' End ',' Step 'in' Body
static std::unique_ptr<ExprAST> ParseForExpr(){
    getNextToken(); // eat for.

    if(CurTok != tok_identifier)
        return LogError("expected identifier after for");
    std::string IdName = IdentifierStr;
    getNextToken(); // eat id.

    if(CurTok != '=')
        return LogError("expected '=' after for");
    getNextToken(); // eat '='

    auto Start = ParseExpression(); // parse start_expr
    if(!Start)
        return nullptr;
    if(CurTok != ',')
        return LogError("expected ',' after for start value");
    getNextToken(); // eat ','

    auto End = ParseExpression(); // parse end_expr
    if(!End)
        return nullptr;

    // the step value is optional
    std::unique_ptr<ExprAST> Step;
    if(CurTok == ','){
        getNextToken(); // eat ','
        Step = ParseExpression(); // parse step_expr
        if(!Step)
            return nullptr;
    }

    if(CurTok != tok_in)
        return LogError("expected 'in' after for");
    getNextToken(); // eat "in".

    auto Body = ParseExpression(); // parse body_expr
    if(!Body)
        return nullptr;

    return std::make_unique<ForExprAST>(IdName, std::move(Start),
                        std::move(End), std::move(Step), std::move(Body));
}

// var_expr
// ::= 'var' id1 '=' expr1 ',' id2 '=' expr2 'in' expr_body
static std::unique_ptr<ExprAST> ParseVarExpr(){
    getNextToken(); // eat the var.
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

    // read at least one varaible
    if(CurTok != tok_identifier)
        return LogError("expected identifier after var");
    while(true){
        std::string Name = IdentifierStr;
        getNextToken(); // eat identifier.
        // 可以选择性地赋初始值
        std::unique_ptr<ExprAST> Init = nullptr;
        if(CurTok == '='){
            getNextToken(); // eat '='.
            Init = ParseExpression();
            if(!Init)
                return nullptr;
        }
        VarNames.push_back(std::make_pair(Name, std::move(Init)));
        if(CurTok != ',')
            break; // End of the list
        getNextToken(); // eat ','
        if(CurTok != tok_identifier)
            return LogError("expected identifier list after var");
    }
    if(CurTok != tok_in)
        return LogError("expected 'in' keyword after 'var'");
    getNextToken(); // eat 'in'.
    auto Body = ParseExpression();
    if(!Body)
        return nullptr;
    
    return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

// primary
// ::= identifier_expr          id or id(expr)
// ::= number_expr              NumVal
// ::= paren_expr               (expr)
// ::= if_expr                  if expr then expr else expr
// ::= for_expr                 for id = expr, expr, expr in expr
// ::= var_expr                 Var id1 = expr, id2 = expr in Body
static std::unique_ptr<ExprAST> ParsePrimary(){
    switch(CurTok){
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_if:
        return ParseIfExpr();
    case tok_for:
        return ParseForExpr();
    case tok_var:
        return ParseVarExpr();
    default:
        return LogError("unknown token when expecting an expression");
    }
}

// unary
// ::= primary
// ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary(){
    // if the current token is not an operator
    if(!isascii(CurTok) || CurTok=='(' || CurTok==',')
        return ParsePrimary();
    
    // if the current token is a unary operator
    int Opc = CurTok;
    getNextToken(); // eat that unary operator
    if(auto Operand = ParseUnary())
        return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
    return nullptr;
}

// bin_op_RHS
// ::= ('+' unary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS){
    while(true){ // ['+', primary], *
        int TokPrec = GetTokPrecedence();
        if(ExprPrec > TokPrec)
            return LHS; // if not exceed previous Precedence, then return.
        
        int BinOp = CurTok;
        getNextToken(); // eat BinOp

        auto RHS = ParseUnary();
        // NumVal or (Expression) or Variable or Func(Expression)
        if(!RHS)
            return nullptr;
        
        int NextPrec = GetTokPrecedence(); // the precednece of next Op
        if(TokPrec < NextPrec){ // the precedence of next Op must exceed current one.
            RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS));
            if(!RHS)
                return nullptr;
        }

        // merge "LHS Op RHS"
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

// expression
// ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression(){
    auto LHS = ParseUnary();
    if(!LHS)
        return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

// prototype
// ::= id '(' id1 ',' id2 ',' ... ')'
// ::= binary LETTER number? '(' id1 ',' id2 ',' ')'
// ::= unary LETTER '(' id1 ')'
static std::unique_ptr<PrototypeAST> ParsePrototype(){
    std::string FnName;
    int Kind; // 0:identifier, 1:unary, 2:binary
    int BinaryPrecedence = 30; // binary Op 的默认优先级
    
    switch(CurTok){
    case tok_identifier:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;
    case tok_unary:
        getNextToken();
        if(!isascii(CurTok))
            return LogErrorP("Expected unary operator");
        FnName = "unary";
        FnName += (char)CurTok;
        Kind = 1;
        getNextToken();
        break;
    case tok_binary:
        getNextToken();
        if(!isascii(CurTok))
            return LogErrorP("Expected binary operator");
        FnName = "binary";
        FnName += (char)CurTok;
        Kind = 2;
        getNextToken();
        // read the precedence if present
        if(CurTok == tok_number){
            if(NumVal<1 || NumVal>100)
                return LogErrorP("Invalid precedence: must be 1..100");
            BinaryPrecedence = NumVal;
            getNextToken();
        }
        break;
    default:
        return LogErrorP("Expected function name in prototype");
    }

    if(CurTok != '(')
        return LogErrorP("Expected '(' in prototype");
    getNextToken(); // eat '('
    std::vector<std::string> ArgNames;
    while(CurTok != ')'){
        if(CurTok == tok_identifier)
            ArgNames.push_back(IdentifierStr);
        else
            return LogErrorP("lack of varaible");
        getNextToken(); // eat identifier
        if(CurTok == ')')
            break;
        if(CurTok != ',')
            return LogErrorP("Expected ')' or ',' in argument list");
        getNextToken(); // eat ','
    }
    getNextToken(); // eat ')'

    if(Kind && (Kind != ArgNames.size()))
        return LogErrorP("Invalid number of operands for operator");
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames),
                                        Kind != 0, BinaryPrecedence);
}

// definition
// ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition(){
    getNextToken(); // eat def.
    auto Proto = ParsePrototype(); // Function head
    if(!Proto)
        return nullptr;
    if(auto E = ParseExpression()) // Function body
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

// top_level_expr
// ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr(){
    if(auto E = ParseExpression()){
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

// external
//::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern(){
    getNextToken(); // eat extern.
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues;
#ifdef ENABLE_JIT
    static std::unique_ptr<KaleidoscopeJIT> TheJIT;
    static std::unique_ptr<FunctionPassManager> TheFPM;
    static std::unique_ptr<LoopAnalysisManager> TheLAM;
    static std::unique_ptr<FunctionAnalysisManager> TheFAM;
    static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
    static std::unique_ptr<ModuleAnalysisManager> TheMAM;
    static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
    static std::unique_ptr<StandardInstrumentations> TheSI;
#endif
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value* LogErrorV(const char *Str){
    LogError(Str);
    return nullptr;
}

// 1. see if the function has already been added to the current module
// 2. check if it exists in the FunctionProtos map, then codegen it
//   （set types,arg_names, then add it to the current module)
Function* getFunction(std::string Name){
    // check if already registered in the current module
    if(auto *F = TheModule->getFunction(Name))
        return F;
    // if not, check whether can codegen from some existing prototype
    auto FI = FunctionProtos.find(Name);
    if(FI != FunctionProtos.end())
        return FI->second->codegen();
    return nullptr; // if no existing prototype
}

static AllocaInst* CreateEntryBlockAlloca(Function* TheFunction, StringRef VarName){
    // 创建了一个 IRBuilder,可以方便地在当前的基本块中插入新的指令。
    // 接受两个参数，一个是指向基本块的指针，另一个是指向基本块中的某个位置的迭代器。
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                    TheFunction->getEntryBlock().begin());
    // 创建一个 alloca 指令，并返回它的指针.
    // 接受三个参数，一个是要分配的类型，一个是要分配的数组大小，一个是要分配的变量名
    return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), nullptr, VarName);
}

Value* NumberExprAST::codegen(){
    // APFloat是LLVM中用于表示浮点数的类, APFloat可以保存任意精度的浮点常量
    // ConstantFP是LLVM中用于表示浮点数常量的类, 是Value的子类
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value* VariableExprAST::codegen(){
    // look up the variable in the fucntion
    AllocaInst* A = NamedValues[Name];
    if(!A)
        return LogErrorV("Unknown varaiable name");
    // load the value
    return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

Value* UnaryExprAST::codegen(){
    Value* OperandV = Operand->codegen();
    if(!OperandV)
        return nullptr;
    Function* F = getFunction(std::string("unary") + Opcode);
    if(!F)
        return LogErrorV("Unknown unary operator");
    return Builder->CreateCall(F, OperandV, "unop");
}

Value* BinaryExprAST::codegen(){
    // special case '='
    if(Op == '='){
        // 将左操作数的指针从 ExprAST 类型转换为 VariableExprAST 类型
        VariableExprAST* LHSE = static_cast<VariableExprAST*>(LHS.get());
        if(!LHSE)
            return LogErrorV("destination of '=' must be a variable");
        Value* Val = RHS->codegen();
        if(!Val)
            return nullptr;
        // look up the name in the NamedValues map
        Value* Variable = NamedValues[LHSE->getName()];
        if(!Variable)
            return LogErrorV("Unknown variable name");
        // 将右侧的值Val存储到左侧名称对应的内存变量中
        Builder->CreateStore(Val, Variable);
        return Val;
    }

    Value* L = LHS->codegen();
    Value* R = RHS->codegen();
    if(!L || !R)
        return nullptr;
    switch(Op){
    case '+':
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // convert bool 0/1 to double 0.0/1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        break;
    }
    Function* F = getFunction(std::string("binary") + Op);
    assert(F && "binary operator not found");
    Value* Ops[] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");
}

Value* CallExprAST::codegen(){
    // look up the Function name in the global module table
    Function* CalleeF = getFunction(Callee); // Args has not yet codegen.
    if(!CalleeF)
        return LogErrorV("Unknown function reference");
    
    // check if the number of arguments is correct.
    if(CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value*> ArgsV;
    // generate Values for arguments
    for(int i=0; i<Args.size(); i++){
        ArgsV.push_back(Args[i]->codegen());
        if(!ArgsV.back())
            return nullptr;
    }
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
    // Value*                 (Function*, vector<Value*>) 
}

Value* IfExprAST::codegen(){
    // 生成条件表达式
    Value* CondV = Cond->codegen();
    if(!CondV)
        return nullptr;

    // 生成比较表达式
    CondV = Builder->CreateFCmpONE(
        CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    // 获取正在构建的当前函数对象 
    // (向Builder询问当前的基本块，再向基本块询问它的父节点(当前嵌入的函数))
    Function* TheFunction = Builder->GetInsertBlock()->getParent();

    // 创建Then, Else, Merge的基本块(标签)，并将Then基本块至于函数末尾
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    // 生成条件跳转表达式
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // 生成ThenBB中的代码
    Builder->SetInsertPoint(ThenBB); // 选择代码插入点为ThenBB
    Value* ThenV = Then->codegen();
    if(!ThenV)
        return nullptr;
    Builder->CreateBr(MergeBB); // 生成到MergeBB的固定跳转语句
    // 获取当前基本块，这样做是为了后面创建 PHI 节点时，能够正确地获取 then 分支的最后一个基本块。
    ThenBB = Builder->GetInsertBlock();
    
    // 生成ElseBB中的代码
    TheFunction->insert(TheFunction->end(), ElseBB); // 将ElseBB基本块插入到函数末尾
    Builder->SetInsertPoint(ElseBB);
    Value* ElseV = Else->codegen();
    if(!ElseV)
        return nullptr;
    Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();

    // 生成MergeBB中的代码
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    // 生成PHI结点
    PHINode* PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");
    // 给PHI结点添加 输入值 和 它来自的基本块 
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

// Output for-loop as:
// loop_header:
//     ...
//     start = start_expr
//     goto loop
// loop:
//     var = phi [start, loop_header], [next_var, loop_end]
//     ...
//     body_expr
//     ...
// loop_end:
//     step = step_expr
//     next_var = var + step
//     end_cond = end_expr
//     br end_cond, loop, out_loop
// out_loop:
Value* ForExprAST::codegen(){
    // 获取当前的函数
    Function* TheFunction = Builder->GetInsertBlock()->getParent();
    // 在entry块中给Var分配内存变量Alloca
    AllocaInst* Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

    //---------------生成PreheaderBB基本块中的代码---------------//
    // 生成Start表达式
    Value* StartVal = Start->codegen();
    if(!StartVal)
        return nullptr;
    // 将Var存入内存空间
    Builder->CreateStore(StartVal, Alloca); 

    // 创建一个基本块(标签)LoopBB 放在当前函数的末尾
    BasicBlock* LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
    // 生成一个到LoopBB的固定跳转表达式
    Builder->CreateBr(LoopBB);

    //---------------生成LoopBB基本块中的代码---------------//
    Builder->SetInsertPoint(LoopBB);

    // 保存VarName对应的old value
    AllocaInst* OldVal = NamedValues[VarName];
    // 用新生成的 PHI Value 作为新的Value
    NamedValues[VarName] = Alloca;

    // 继续生成LoopBB基本块中的代码: Body
    if(!Body->codegen())
        return nullptr;
    
    // 若Step存在, codegen, 若不存在, 用1.0代替
    Value* StepVal;
    if(Step){
        StepVal = Step->codegen();
        if(!StepVal)
            return nullptr;
    }else{
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    // reload, increment, and restore the alloc
    Value* CurVar = Builder->CreateLoad(Alloca->getAllocatedType(),
                                    Alloca, VarName.c_str());
    // 根据Variable和StepVal生成NextVal
    Value* NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
    Builder->CreateStore(NextVar, Alloca);

    // 生成结束条件表达式
    Value* EndCond = End->codegen();
    if(!EndCond)
        return nullptr;
    // 从布尔值转化为0.0/1.0
    EndCond = Builder->CreateFCmpONE(
        EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    // 创建一个基本块AfterBB，放在函数的末尾
    BasicBlock* AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);
    // 生成条件跳转语句，根据EndCond决定跳转的目的地
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    //---------------生成AfterBB基本块中的代码---------------//
    Builder->SetInsertPoint(AfterBB);
    // 恢复NamedValues中的VarName映射
    if(OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);

    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Value* VarExprAST::codegen(){
    std::vector<AllocaInst*> OldBindings;
    Function* TheFunction = Builder->GetInsertBlock()->getParent();

    for(int i=0; i<VarNames.size(); i++){
        const std::string &VarName = VarNames[i].first;
        ExprAST* Init = VarNames[i].second.get();
        // Codegen Var
        Value* InitVal;
        if(Init){ // 指定了初值的情况
            InitVal = Init->codegen();
            if(!InitVal)
                return nullptr;
        }else{ // 没有指定初值的情况
            InitVal = ConstantFP::get(*TheContext, APFloat(0.0));
        }
        // 创建内存变量
        AllocaInst* Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
        // 将Var存入内存
        Builder->CreateStore(InitVal, Alloca);
        // 用当前的Var替换NamedValues中的值
        OldBindings.push_back(NamedValues[VarName]);
        NamedValues[VarName] = Alloca;
    }
    // Codegen Body
    Value* BodyVal = Body->codegen();
    if(!BodyVal)
        return nullptr;
    
    // 恢复NamedValues中的原值
    for(int i=0; i<VarNames.size(); i++)
        NamedValues[VarNames[i].first] = OldBindings[i];
    return BodyVal;
}

// 1. get function type: double(double, double, ...)
// 2. register function (type, name)
// 3. set the names of function arguments
Function* PrototypeAST::codegen(){
    std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
    // get function type: double(double, double)
    FunctionType *FT = 
        FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
    
    // set fucntion type and function name, insert into TheModule
    Function *F = 
        Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
    // 实际创建了对应原型的函数IR,这里指定了函数类型，链接形式和要插入的模块。
    // external linkage标识这个函数可能被定义在当前模块之外并且/或它可以被外部模块所调用
    // 传入的名称是用户指定的名称：当TheModule指定时，此名就在在TheModules的符号表中注册了。

    // set names for all arguments
    int Idx = 0;
    for(auto &Arg : F->args())
        Arg.setName(Args[Idx++]);
    return F;
}

// 1. codegen prototype
// 2. create basic block
// 3. record the function arguments in the NamedValues map
// 4. codegen the body(RetVal) of function
Function *FunctionAST::codegen(){
    // write Proto name into FunctionProtos map 
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    if(!TheFunction)
        return nullptr;
    
    // 如果是一个binary operator，注册它的优先级
    if(P.isBinaryOp())
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // Create a new basic block to start insertion into.
    BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB); // 告诉Builder新的指令应当被插入在新的基本块后面

    // record the function arguments in the NamedValues map
    NamedValues.clear();
    for(auto &Arg : TheFunction->args()){
        // 创建变量的内存空间
        AllocaInst* Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());
        // 将函数参数存入内存空间
        Builder->CreateStore(&Arg, Alloca);
        // 将变量在内存空间的索引Alloca注册进NamedValues
        NamedValues[std::string(Arg.getName())] = Alloca;
    }

    // codegen the body of function
    if(Value* RetVal = Body->codegen()){
        // Finish off the function.
        Builder->CreateRet(RetVal);
        // validate the generated code, check for consistency
        verifyFunction(*TheFunction);
#ifdef ENABLE_JIT
        // Run the optimizer on the function
        TheFPM->run(*TheFunction, *TheFAM);
#endif
        return TheFunction;
    }
    // Error reading body, remove function.
    TheFunction->eraseFromParent();

    if(P.isBinaryOp())
        BinopPrecedence.erase(P.getOperatorName());
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager(){
    // 创建一个新的LLVM上下文（LLVMContext）对象，LLVM上下文是用于管理LLVM编译器的状态和资源的对象
    TheContext = std::make_unique<LLVMContext>();
    // 创建一个新的模块对象，并使用给定的名称和LLVM上下文进行初始化
    TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
    
    // 创建一个新的IRBuilder对象，IRBuilder是LLVM中用于构建中间表示（IR）指令的工具类。
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
#ifdef ENABLE_JIT
    // 设置模块的数据布局，用于指定数据类型在内存中的布局方式。这里使用TheJIT对象的数据布局来设置模块的数据布局。
    TheModule->setDataLayout(TheJIT->getDataLayout());

    // 创建函数pass管理器，用于管理和应用一系列优化pass到函数的中间表示
    TheFPM = std::make_unique<FunctionPassManager>();
    // 创建循环分析管理器，用于管理和执行与循环相关的分析操作
    TheLAM = std::make_unique<LoopAnalysisManager>();
    // 创建函数分析管理器，用于管理和执行与函数相关的分析操作
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    // 创建CGSCC分析管理器，用于分析函数调用图（Call Graph）的强连通分量（Strongly Connected Component）
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    // 创建模块分析管理器，用于管理和执行与模块相关的分析操作
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    
    // PassInstrumentationCallbacks and StandardInstrumentations
    // are required for the pass instrumentation framework, 
    // which allows developers to customize what happens between passes.
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext, /*DebugLogging*/ true);

    // 将PassInstrumentationCallbacks和模块分析管理器 注册到StandardInstrumentations
    TheSI->registerCallbacks(*ThePIC, TheMAM.get());
    
    // Add transform passes.
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->addPass(InstCombinePass());
    // Reassociate expressions.
    TheFPM->addPass(ReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->addPass(GVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->addPass(SimplifyCFGPass());

    // register analysis passes used in these transform passes
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM); // 注册模块分析
    PB.registerFunctionAnalyses(*TheFAM); // 注册函数分析
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM); // 通过交叉注册代理，建立这些分析之间的关联
#endif
}

static void HandleDefinition(){
    if(auto FnAST = ParseDefinition()){
        if(auto *FnIR = FnAST->codegen()){
            fprintf(stderr, "Read function definition:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
#ifdef ENABLE_JIT
            ExitOnErr(TheJIT->addModule( // 传输新定义的函数到JIT
                ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndPassManager(); // 初始化新的module和pass manager
#endif
        }
    }else{
        getNextToken(); // skip token for error recovery
    }
}

static void HandleExtern(){
    if(auto ProtoAST = ParseExtern()){
        if(auto *FnIR = ProtoAST->codegen()){
            fprintf(stderr, "Read extern:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }else{
        getNextToken(); // skip token for error recovery
    }
}

static void HandleTopLevelExpression(){
    // evaluate a top level expresson into a anonymous function
    if(auto FnAST = ParseTopLevelExpr()){
#ifdef ENABLE_JIT
        if(auto *FnIR = FnAST->codegen()){
            fprintf(stderr, "Top Level Expression:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            // Create a ResourceTracker to track JIT'd memory allocated to our
            // anonymous expression -- that way we can free it after executing.
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();
            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));

            // JIT the module containing the anonymous expression
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndPassManager(); // 初始化(清空)TheModule

            // search the JIT for the __anon_expr_symbol
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
            
            // get the address of the anonymous fucntion
            double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
            fprintf(stderr, "Evaluated to %f\n", FP());

            // delete the anonymous expression module from JIT
            ExitOnErr(RT->remove());
        }
#else
        FnAST->codegen();
#endif
    }else{
        getNextToken(); // skip token for error recovery
    }
}

// top
// ::= definition | external | expression | ';'
static void MainLoop(){
    while(true){
        fprintf(stderr, "ready> ");
        switch (CurTok){
        case tok_eof:
            return;
        case ';':
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(){
#ifdef ENABLE_JIT
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
#endif
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['='] = 2;
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    // initialize JIT
#ifdef ENABLE_JIT
    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
#endif

    InitializeModuleAndPassManager();
    MainLoop();

#ifndef ENABLE_JIT
    // 初始化发出object code的所有targets
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();
    
    // 获取当前机器的目标三元组
    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    // 使用目标三元组来获取一个Target，看LLVM是否支持该三元组
    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error); // 查找对应的目标机器
    if(!Target){
        errs()<<Error;
        return 1;
    }

    // 创建目标机器对象，指定目标三元组，CPU 类型，特性，选项和重定位模式
    auto CPU = "generic";
    auto Features = "";
    TargetOptions opt;
    auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, Reloc::PIC_);

    // 配置模块，以指定目标和数据布局
    TheModule->setDataLayout(TargetMachine->createDataLayout());

    // 创建一个输出流对象，指定输出文件名，错误码和文件选项
    auto Filename = "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);
    if(EC){
        errs()<<"Could not open file: "<<EC.message();
        return 1;
    }

    // 定义一个发出对象代码的过程
    legacy::PassManager pass; // 创建一个传递管理器对象，用于管理 LLVM 的优化和转换传递
    auto FileType = CodeGenFileType::ObjectFile;
    // 向传递管理器添加一系列的传递，用于将模块转换成目标文件类型的输出流
    if(TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)){
        errs() << "TargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*TheModule); // 执行传递管理器中的所有传递，并将模块的内容写入输出流
    dest.flush(); // 刷新输出流，并关闭文件
    outs()<<"Wrote "<<Filename<<"\n";
#endif

    return 0;
}