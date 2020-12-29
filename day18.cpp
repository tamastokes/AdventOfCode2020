#include <iostream>
#include <fstream>
#include <string>
#include <vector>

class node{
    protected:
        int _type; // 0: num, 1: sum, 2: mul, 3: ob, 4: cb
        node *left;
        node *right;
        node *parent;
    public:
        node(int t, node *l=0, node *r=0, node *p=0) : _type(t), left(l), right(r), parent(p) {}
        virtual long long evaluate() = 0;
        virtual int type() { return _type; }
};

class num_node : public node{
    private:
        long long n;
    public:
        num_node(long long val) : node(0), n(val) {}
        long long evaluate(){ return n; }

};

class sum_node : public node{
    public:
        sum_node(node *left) : node(1, left){}
        long long evaluate(){ return left->evaluate() + right->evaluate(); }

};

int main(int argc, char *argv[]){
    std::ifstream f(argv[1]);
    std::string s;
    std::getline(f, s);

    std::vector<node*> lin;

    for (int i=0; i<s.size(); ++i)
    {
        if (s[i] == ' '){
            // skip
        }
        else if(s[i] == '+'){
            // previous op was a +
            if (lin.size()>=2 && lin[lin.size()-2]->type() == 1){
            }
            else{
                node *left = lin.back();
                lin.pop_back();
                lin.push_back(new sum_node(left)); 
            }
        }
        else if(s[i] == '*'){
        }
        else if(s[i] == '('){
        }
        else if(s[i] == ')'){
        }
        else{
            int n = s[i]-'0';
            lin.push_back(new num_node(n));
        }
    }
    std::cout << lin.back()->evaluate() << std::endl;
}
