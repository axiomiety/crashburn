#include <iostream>

int main()
{
    int i = 42, j=24;
    int *p = nullptr;
    std::cout << p << i << j << std::endl;
    p = &i;
    std::cout << p << i << j << std::endl;
    p = &j;
    std::cout << p << i << j << std::endl;
    *p = 36;
    std::cout << p << i << j << std::endl;
}