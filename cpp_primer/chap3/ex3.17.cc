#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::vector;

void to_upper(string& s){
    // it's important to get a ref here!
    for (auto &c : s)
        c = toupper(c);
}
int main()
{
    vector<string> svec;
    string item;
    while (cin >> item)
        svec.push_back(item);
    
    uint count = 8;
    //for (auto s = svec.begin(); s != svec.end(); ++s) {
    uint idx = 0;
    for (auto &s : svec) {
        to_upper(s);
        cout << s << "," << svec[idx] << " ";
        --count;
        if (count == 0) {
            cout << endl;
            count = 8;
        }
        ++idx;
    }
    return 0;
}