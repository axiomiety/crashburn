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

struct WordSizeCounter {
    void process(istream& is) {
        string line;
        while(getline(is, line)) {
            stringstream ss(line);
            string token;
            while(getline(ss, token, ' '))
                counter[token.size()] += 1;
        }
    }
    map<int, int> counter;
};

int main(int argc, char **argv)
{
    auto wsc = WordSizeCounter();
    ifstream is(argv[1]);
    if (!is.is_open()) {
        cerr << argv[1] << " could not be opened" << endl;
        return EXIT_FAILURE;
    }

    wsc.process(is);
    for (const auto& pair : wsc.counter)
        cout << pair.first << " " << pair.second << endl;
    return EXIT_SUCCESS;
}