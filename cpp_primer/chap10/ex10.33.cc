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
using std::back_inserter;
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::cerr;
using std::deque;
using std::end;
using std::endl;
using std::fill_n;
using std::forward_list;
using std::getline;
using std::ifstream;
using std::ofstream;
using std::initializer_list;
using std::istream;
using std::istringstream;
using std::iterator;
using std::list;
using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;
using namespace std::placeholders;

int main(int argc, char **argv)
{
    if (argc != 4)
    {
        cerr << "expecting 3 arguments" << endl;
      return EXIT_FAILURE;
    }
    ifstream input(argv[1]);
    ofstream output_even(argv[2]);
    ofstream output_odd(argv[3]);
    std::istream_iterator<int> int_iter(input), eof;
    std::ostream_iterator<int> even_iter(output_even, "\n");
    std::ostream_iterator<int> odd_iter(output_odd, "\n");

    while (int_iter != eof)
    {
        if (*int_iter %2 == 0)
            even_iter = *int_iter++;
        else
            odd_iter = *int_iter++;
    }

    return EXIT_SUCCESS;
}