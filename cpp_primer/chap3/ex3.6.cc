#include <iostream>
#include <string>
using std::string;

int main()
{
    string s("foobar");
    for (auto &c:s)
        c = 'x';
    std::cout << s << std::endl;
    return 0;
}