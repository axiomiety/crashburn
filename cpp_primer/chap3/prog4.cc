#include <iostream>
#include <string>
using std::cin;
using std::cout;
using std::endl;
using std::string;
int main()
{
    string line;
    string foo(20,'f');
    // doesn't work because a string literal is a const char[]
    //string s = "ab" + "cd";
    cout << foo << endl;
    while (getline(cin, line)) {
        for (auto c : line)
            cout << c << endl;
    }
    return 0;
}