#include <iostream>
#include "Sales_item.h"

// gcc prog.cc -lstdc++ -I. -o prog && ./prog
int main()
{
    Sales_item total, placeholder;
    bool init = false;
    while (std::cin >> placeholder) {
        if (init)
            total += placeholder;
        else {
            total = placeholder;
            init = true;
        }
    }
    std::cout << total << std::endl;

    return 0;
}