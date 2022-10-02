#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::getline;
using std::ifstream;
using std::initializer_list;
using std::istream;
using std::istringstream;
using std::map;
using std::string;
using std::vector;

void my_read(string fname, vector<string> *svec)
{
    string line;
    ifstream input(fname);
    if (input)
    {
        while (getline(input, line))
        {
            svec->push_back(line);
        }
    }
    else
    {
        cerr << "could not open " + fname << " for reading" << endl;
    }
}

int main(int argc, char **argv)
{
    vector<string> *svec = new vector<string>();

    my_read(string(argv[1]), svec);
    for (const auto &line : *svec)
    {
        istringstream record(line);
        string token;
        while (record >> token)
            cout << token << endl;
    }
    return EXIT_SUCCESS;
}