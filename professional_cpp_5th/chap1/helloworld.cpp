/*
so this was funky... i had to download gcc through brew (v12)
and run the below to convert std libs to modules o_O

-std=c++20 -fmodules-ts -xc++-system-header iostream
*/
import <iostream>;

int main()
{
    std::cout << "Hello, World!" << std::endl;
    return 0;
}