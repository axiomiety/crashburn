#include <iostream>
#include <map>
#include "Sales_item.h"


//gcc ex1.23.cc -std=c++17 -lstdc++ -I. -o prog && ./prog < 2books.txt
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