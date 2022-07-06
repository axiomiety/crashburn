#include <iostream>
#include "Sales_item.h"

// gcc prog6.cc -lstdc++ -I. -o prog && ./prog
int main()
{
    Sales_item book;
    std::cin >> book;
    std::cout << book << std::endl;

    return 0;
}