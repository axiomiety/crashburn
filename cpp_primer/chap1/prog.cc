#include <iostream>

// with gcc, you need to link this with -lstdc++ - otherwise iostream won't be defined
int main()
{
    std::cout << "Enter two numbers:" << std::endl;
    int v1 = 0, v2 = 0;
    std::cin >> v1 >> v2;
    std::cout <<"The sum of " << v1 << " and " << v2 << " is " << v1+v2 <<std::endl;
    return 0;
}