#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::initializer_list;
using std::map;
using std::string;
using std::vector;

int funnyAdd(int a, int b)
{
    return a + b + 42;
}

int main(int argc, char **argv)
{
    using FP = int (*)(int, int);
    vector<FP> vfp;
    vfp.push_back(funnyAdd);
    vfp.push_back(&funnyAdd);
    for (auto const &fp : vfp)
        cout << fp(1, 2) << endl;
    return EXIT_SUCCESS;
}