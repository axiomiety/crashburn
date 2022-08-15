#include <iostream>
#include <string>
#include <vector>
#include <map>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::map;
using std::string;
using std::vector;

int main()
{
    string s;
    map<string, int> counter;
    bool prev_char_was_f = false;
    while (cin >> s)
    {
        for (const auto &c : s)
        {
            auto cc = tolower(c);
            switch (cc)
            {
            case 'a':
            case 'e':
            case 'i':
            {
                if (prev_char_was_f)
                {
                    auto str = string("f") + string(1, cc);
                    auto emplace_pair = counter.emplace(str, 0);
                    emplace_pair.first->second += 1;
                    prev_char_was_f = false;
                    break;
                }
                prev_char_was_f = false;
            }
            case 'o':
            case 'u':
            {
                auto str = string(1, cc);
                auto emplace_pair = counter.emplace(str, 0);
                emplace_pair.first->second += 1;
            }
            break;
            case 'f':
            {
                if (prev_char_was_f)
                {
                    auto str = string(2, cc);
                    auto emplace_pair = counter.emplace(str, 0);
                    emplace_pair.first->second += 1;
                }
                else
                    prev_char_was_f = true;
            }
            break;
            case 'l':
            {
                if (prev_char_was_f)
                {
                    auto str = string("f") + string(1, cc);
                    auto emplace_pair = counter.emplace(str, 0);
                    emplace_pair.first->second += 1;
                }
                prev_char_was_f = false;
            }
            break;
            default:
                prev_char_was_f = false;
            }
        }
    }
    for (auto it = counter.cbegin(); it != counter.cend(); ++it)
        cout << (*it).first << "=" << (*it).second << endl;
}