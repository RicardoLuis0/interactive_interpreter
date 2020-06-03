#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <cmath>

bool is_num(std::string s){
    bool dot=false;//numbers with multiple dots are malformed
    bool first=false;
    for(char c:s){
        if(c=='.'&&!dot){
            dot=true;
        }else if(c<'0'||c>'9'){
            return false;
        }
        if(first)first=false;
    }
    return true;
}

bool is_ident(std::string s){
    if(s[0]>='0'&&s[0]<='9')return false;
    for(char c:s){
        if((c<'a'||c>'z')&&(c<'A'||c>'Z')&&(c<'0'&&c>'9')&&c!='_')return false;
    }
    return true;
}

double parse_num(std::string s){
    return std::stod(s);
}

bool is_op(char c){
    return c=='+'||c=='-'||c=='*'||c=='/'||c=='%'||c=='^';
}

bool is_separator(char c){
    return (c==' '||c=='\n'||c=='\t'||c=='\r');
}

struct token{
    enum TK_TYPE{
        TK_PAREN_OPEN,
        TK_PAREN_CLOSE,
        TK_ASSIGN,
        TK_OP,
        TK_NUM,
        TK_IDENT,
    }type;
    std::string data;
};

void commit(bool &reading,std::string &tmp,std::vector<token> &tks){
    if(reading){
        reading=false;
        if(is_num(tmp)){
            tks.push_back((token){token::TK_NUM,tmp});
        }else if(is_ident(tmp)){
            tks.push_back((token){token::TK_IDENT,tmp});
        }else{
            throw std::runtime_error("unexpected '"+tmp+"'");
        }
        tmp="";
    }
}

std::vector<token> scan(const std::string& s){
    std::vector<token> tks;
    std::string tmp;
    bool reading=false;
    for(char c:s){
        if(is_separator(c)){
            commit(reading,tmp,tks);
        }else if(c=='('){
            commit(reading,tmp,tks);
            tks.push_back({token::TK_PAREN_OPEN,std::string(1,c)});
        }else if(c==')'){
            commit(reading,tmp,tks);
            tks.push_back({token::TK_PAREN_CLOSE,std::string(1,c)});
        }else if(c=='='){
            commit(reading,tmp,tks);
            tks.push_back({token::TK_ASSIGN,std::string(1,c)});
        }else if(is_op(c)){
            commit(reading,tmp,tks);
            tks.push_back({token::TK_OP,std::string(1,c)});
        }else{
            reading=true;
            tmp+=c;
        }
    }
    if(tmp.size()>0){
        commit(reading,tmp,tks);
    }
    return tks;
}

struct node {
    virtual double get_value(std::unordered_map<std::string,double>&)=0;
    virtual ~node(){
    }
};

struct num : public node {
    double data;
    num(double d):data(d){}
    virtual double get_value(std::unordered_map<std::string,double>&) override {
        return data;
    }
};

struct var : public node {
    std::string name;
    bool negative;
    var(std::string s,bool neg):name(s),negative(neg){}
    virtual double get_value(std::unordered_map<std::string,double> &memory) override {
        try{
            return negative?-memory.at(name):memory.at(name);
        }catch(...){
            throw std::runtime_error("undefined variable '"+name+"'");
        }
    }
};

int operator_precedence(char c){
    switch(c){
    case '+':
        return 1;
    case '-':
        return 1;
    case '*':
        return 2;
    case '/':
        return 2;
    case '%':
        return 2;
    case '^':
        return 3;
    }
    throw std::runtime_error("unknown operator '"+std::string(1,c)+"'");
}

struct negexprgrp : public node {//negative expression group
    std::unique_ptr<node> expr;
    negexprgrp(const std::vector<token> tks,uint32_t &i);
    virtual double get_value(std::unordered_map<std::string,double> &memory) override;
};

struct expression : public node {//expression
    
    struct expr_data{//data or 
        expr_data(std::unique_ptr<node> f):factor(std::move(f)),is_op(false){}
        expr_data(char c):op(c),is_op(true){}
        char op;
        std::unique_ptr<node> factor;
        bool is_op;
    };
    
    std::vector<expr_data> expr;
    
    expression(const std::vector<token> tks,uint32_t &i){
        read_expr(tks,i,false);
    }
    expression(const std::vector<token> tks,uint32_t &i,bool b){
        read_expr(tks,i,b);
    }
    void read_expr(const std::vector<token> tks,uint32_t &i,bool parens){
        std::vector<char> op_stack;
        for(;;i++){
            if(i>=tks.size()){
                throw std::runtime_error("expected value, got EOF");
            }
            switch(tks[i].type){//read value
                case token::TK_IDENT:
                    expr.emplace_back(std::make_unique<var>(tks[i].data,false));
                    break;
                case token::TK_NUM:
                    expr.emplace_back(std::make_unique<num>(parse_num(tks[i].data)));
                    break;
                case token::TK_OP:
                    if((i+1)<tks.size()){
                        if(tks[i+1].type==token::TK_NUM||tks[i+1].type==token::TK_IDENT){//allow sign modifiers
                            if(tks[i].data[0]=='-'){//negative
                                i++;
                                if(tks[i].type==token::TK_NUM){
                                    expr.emplace_back(std::make_unique<num>(-parse_num(tks[i].data)));
                                }else{
                                    expr.emplace_back(std::make_unique<var>(tks[i].data,true));
                                }
                                break;
                            }else if(tks[i].data[0]=='+'){//positive (no change)
                                i++;
                                if(tks[i].type==token::TK_NUM){
                                    expr.emplace_back(std::make_unique<num>(parse_num(tks[i].data)));
                                }else{
                                    expr.emplace_back(std::make_unique<var>(tks[i].data,false));
                                }
                                break;
                            }
                        }else if(tks[i+1].type==token::TK_PAREN_OPEN){
                            if(tks[i].data[0]=='-'){//handle negative expression groups
                                i++;
                                expr.emplace_back(std::make_unique<negexprgrp>(tks,i));
                                break;
                            }
                        }
                    }
                    //if no sign modifiers found, throw
                    throw std::runtime_error("expected value, got '"+tks[i].data+"'");
                case token::TK_PAREN_OPEN:
                    i++;
                    read_expr(tks,i,true);
                    break;
                case token::TK_PAREN_CLOSE:
                    throw std::runtime_error("expected value, got ')'");
                default:
                    throw std::runtime_error("expected value, got '"+tks[i].data+"'");
            }
            i++;
            if(i<tks.size()){//read opeartor
                if(tks[i].type==token::TK_OP){
                    int precedence=operator_precedence(tks[i].data[0]);
                    while(op_stack.size()>0&&operator_precedence(op_stack.back())>=precedence){//shunting yard
                        expr.emplace_back(op_stack.back());
                        op_stack.pop_back();
                    }
                    op_stack.push_back(tks[i].data[0]);
                }else{
                    if(parens){
                        while(op_stack.size()>0){
                            expr.emplace_back(op_stack.back());
                            op_stack.pop_back();
                        }
                        if(expr.size()==0)throw std::runtime_error("empty expression");
                        return;
                    }else{
                        throw std::runtime_error("expected operator, got '"+tks[i].data+"'");
                    }
                }
            }else{
                if(parens){
                    throw std::runtime_error("expected ')', got EOF");
                }else{
                    while(op_stack.size()>0){
                        expr.emplace_back(op_stack.back());
                        op_stack.pop_back();
                    }
                    if(expr.size()==0)throw std::runtime_error("empty expression");
                    return;
                }
            }
        }
    }
    static double operate(double lhs,double rhs,char op){//execute operation
        switch(op){
            case '+':
                return lhs+rhs;
            case '-':
                return lhs-rhs;
            case '/':
                return lhs/rhs;
            case '*':
                return lhs*rhs;
            case '%':
                return fmod(lhs,rhs);
            case '^':
                return pow(lhs,rhs);
        }
        throw std::runtime_error("invalid operator");
    }
    virtual double get_value(std::unordered_map<std::string,double> &memory) override {
        std::vector<double> stack;
        for(auto &e:expr){//evaluate RPN
            if(e.is_op){
                double rhs=stack.back();
                stack.pop_back();
                double lhs=stack.back();
                stack.pop_back();
                stack.push_back(operate(lhs,rhs,e.op));
            }else{
                stack.push_back(e.factor->get_value(memory));
            }
        }
        if(stack.size()!=1)throw std::runtime_error("internal error, invalid stack size");
        return stack.back();
    }
};

negexprgrp::negexprgrp(const std::vector<token> tks,uint32_t &i){
    if(tks[i].type!=token::TK_PAREN_OPEN)throw std::runtime_error("expected '(', got '"+tks[i].data+"'");
    i++;
    expr=std::make_unique<expression>(tks,i,true);
}

double negexprgrp::get_value(std::unordered_map<std::string,double> &memory){
    return -expr->get_value(memory);
}


struct assignment : public node {
    std::string name;
    std::unique_ptr<expression> expr;
    assignment(const std::vector<token> tks,uint32_t &i){
        if(tks[0].type!=token::TK_IDENT){
            throw std::runtime_error("can't assign to '"+tks[0].data+"'");
        }
        name=tks[0].data;
        i=2;
        if(i<tks.size()){
            expr=std::make_unique<expression>(tks,i);
        }else{
            throw std::runtime_error("expected expression, got EOF");
        }
    }
    virtual double get_value(std::unordered_map<std::string,double> &memory) override {
        return memory[name]=expr->get_value(memory);
    }
};

struct assignment_op : public node {
    std::string name;
    std::unique_ptr<expression> expr;
    char op;
    assignment_op(const std::vector<token> tks,uint32_t &i){
        if(tks[0].type!=token::TK_IDENT){
            throw std::runtime_error("can't assign to '"+tks[0].data+"'");
        }
        name=tks[0].data;
        op=tks[1].data[0];
        i=3;
        if(i<tks.size()){
            expr=std::make_unique<expression>(tks,i);
        }else{
            throw std::runtime_error("expected expression, got EOF");
        }
    }
    virtual double get_value(std::unordered_map<std::string,double> &memory) override {
        double lhs;
        try{
            lhs=memory.at(name);
        }catch(...){
            throw std::runtime_error("undefined variable '"+name+"'");
        }
        double rhs=expr->get_value(memory);
        return memory[name]=expression::operate(lhs,rhs,op);
    }
};

std::unique_ptr<node> parse(const std::vector<token> tks){
    uint32_t i=0;
    if(tks.size()>1&&tks[1].type==token::TK_ASSIGN){//assignment
        return std::make_unique<assignment>(tks,i);
    }else if(tks.size()>2&&tks[1].type==token::TK_OP&&tks[2].type==token::TK_ASSIGN){//compound assignment
        return std::make_unique<assignment_op>(tks,i);
    }else{//expression
        return std::make_unique<expression>(tks,i);
    }
}

double interpret(std::string expression) {
    static std::unordered_map<std::string,double> memory;
    return parse(scan(expression))->get_value(memory);
}

//---

#include <iostream>
#include <vector>

std::vector<std::string> separate(std::string s){
    std::vector<std::string> out;
    std::string tmp;
    for(char c:s){
        if(c==';'){
            out.push_back(tmp);
            tmp="";
        }else{
            tmp+=c;
        }
    }
    if(tmp.size()>0){
        out.push_back(tmp);
    }
    return out;
}

bool isEmpty(std::string s){
    for(char c:s){
        if(!(c==' '||c=='\n'||c=='\t'||c=='\r'))return false;
    }
    return true;
}

int main(){
    std::vector<std::string> sv;
    while(true){
        std::string s;
        std::cout<<"> ";
        getline(std::cin,s);
        if(s==":q")break;
        sv=separate(s);
        if(sv.size()>1){
            for(std::string se:sv){
                if(!isEmpty(se)){
                    std::cout<<":"<<se<<"\n";
                    try{
                        std::cout<<interpret(se)<<"\n";
                    }catch(std::exception &e){
                        std::cout<<"Error: "<<e.what()<<"\n";
                    }
                }
            }
        }else{
            if(!isEmpty(s)){
                try{
                    std::cout<<interpret(s)<<"\n";
                }catch(std::exception &e){
                    std::cout<<"Error: "<<e.what()<<"\n";
                }
            }
        }
    }
}

