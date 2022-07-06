#include <iostream>

int main()
{
    int start, end;
    std::cout << "Enter a start, end: " << std::endl;
    std::cin >> start >> end;
    for (int v = start; start > end ? v >= end : v <= end ; start > end ? --v : ++v) {
        std::cout << v << std::endl;
    }
    return 0;
}