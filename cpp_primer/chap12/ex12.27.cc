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

class QueryResult;

class TextQuery
{
    public:
        TextQuery(ifstream&);
        QueryResult query(string) const;

    private:
        string s;
};

void runQueries(ifstream& infile)
{
    TextQuery tq(infile);

}

int main(int argc, char **argv)
{
    // our input file
    ifstream in(argv[1]);

    return EXIT_SUCCESS;
}