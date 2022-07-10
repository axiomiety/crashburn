#include <iostream>

int main()
{
    std::cout << "Who goes with F\145rgus?\012" << std::endl;
    std::cout << 3.14e1L << std::endl;
    std::cout <<  1024.0f << std::endl; //1024f doesn't work somehow
    std::cout <<  3.14L << std::endl;
}