#include <iostream>

int main()
{
    const int v2=0;
    int v1 = v2;
    int *pi = &v1, &r1 = v1;
    const int *p2 = &v2, *const p3 = &i, &r2 = v2;
}