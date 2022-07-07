#include <iostream>
#include <map>
#include "Sales_item.h"


// gcc prog.cc -lstdc++ -I. -o prog && ./prog
int main()
{
    std::map <std::string, Sales_item> ledger;
    Sales_item placeholder;
    
    while (std::cin >> placeholder) {
        if (ledger.count(placeholder.isbn())) {
            ledger[placeholder.isbn()] += placeholder;
        } else {
            ledger[placeholder.isbn()] = placeholder;
        }
    }
    for (auto [key, val]: ledger) {
        std::cout << val << std::endl;
    }

    return 0;
}