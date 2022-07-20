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
    for (decltype(s.size()) index = 0; index != s.size() && !isspace(s[index]); ++index)
        s[index] = toupper(s[index]);
    cout << s << endl;
    return 0;
}