#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
#include <forward_list>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
#include <memory>
#include <algorithm>
#include <iterator>
#include <functional>
#include <map>
#include <set>
#include <cassert>
#include <memory>

using namespace std;

struct PrintString {

    string operator()(istream& ins) const {
        string s("");
        if (ins >> s)
            return s;
        return s;
    }

};

int main(int argc, char **argv)
{
    PrintString p;
    cout << p(cin) << endl;
    return EXIT_SUCCESS;
}