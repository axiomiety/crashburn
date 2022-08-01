#include <iostream>
#include <string>
#include <vector>
#include <map>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;
using std::vector;
using std::string;
using std::map;

int main()
{
    string s;
    map<char, int> counter;
    while (cin >> s) {
        for (const auto &c : s) {
            auto cc = tolower(c);
            switch (cc) {
                case 'a':
                case 'e':
                case 'i':
                case 'o':
                case 'u': {
                    auto search = counter.find(cc);
                    if (search != counter.end()) {
                        counter[cc] += 1;
                    } else {
                        counter[cc] = 1;
                    }
                }
                break;
            }
        }
    }
    for (auto it = counter.cbegin(); it != counter.cend(); ++it)
        cout << (*it).first << "=" << (*it).second << endl;
}