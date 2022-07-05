#include <iostream>

int main()
{
    int start, end;
    std::cout << "Enter a start, end - such that start <= end: " << std::endl;
    std::cin >> start >> end;
    for (int v = start; v <= end; ++v) {
        std::cout << v << std::endl;
    }
    return 0;
}