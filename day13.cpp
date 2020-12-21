#include <iostream>

int main()
{
    // unsigned long long v[] = { 7, 13, 59, 31, 19 };
    // unsigned long long offsets[] = { 0, 1, 4, 6, 7 };
    unsigned long long v[] = { 13, 41, 997, 23, 19, 29, 619, 37, 17 };
    unsigned long long offsets[] = { 0, 3, 13, 22, 32, 42, 44,  50, 61 };
    unsigned long long i = v[0];
    while(true)
    {
        bool ok = true;
        for (unsigned long long j=1; j<(sizeof(v)/sizeof(unsigned long long)); ++j)
        {
            if ((i+offsets[j]) % v[j] != 0){
                ok = false;
                break;
            }
        }
        if (ok){
            std::cout << "Solved: " << i << std::endl;
            return 0;
        }
        if (i % 1000000 == 0) std::cout << i << std::endl;
        i += v[0]; 
    }
}

