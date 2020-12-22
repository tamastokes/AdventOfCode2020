#include <unordered_map>
#include <vector>
#include <iostream>

int main()
{
    std::vector<int> v{0,6,1,7,2,19,20};
    std::unordered_map<int,int> m;
    for (int i=0; i<v.size()-1; ++i){
        m[v[i]] = i+1;
    }
    int curr = v.back();

    for (int i=v.size(); i<30000000; ++i)
    {
        int next;
        auto occurance = m.find(curr);
        if (occurance == m.end())
        {
            next = 0;
        }
        else
        {
            int last = occurance->second; 
            int diff = i - last;
            next = diff;
        }
        m[curr] = i;
        curr = next;
        if (i % 100000 == 0){
            std::cout << i << std::endl;
        }
    }
    std::cout << curr << std::endl;
}

