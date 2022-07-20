#include <iostream>
#include <string>
using std::cin;
using std::cout;
using std::endl;
using std::string;
int main()
{
    string s("Hello World!!!");
    decltype(s.size()) punct_cnt = 0;
    for (auto &c: s)
        c = toupper(c);
    cout << s << endl;
    return 0;
}