#include <iostream>

int main()
{
    int i = 42;
    const int v2=0;
    int v1 = v2;
    int *pi = &v1, &r1 = v1;
    const int *p2 = &v2, *const p3 = &i, &r2 = v2;

    r1 = v2;
    r1 = 3;
    std::cout << v1 << v2 << i << std::endl;
}