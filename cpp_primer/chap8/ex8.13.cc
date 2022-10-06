#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
#include <memory>
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
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;

struct PersonInfo
{
    string name;
    vector<string> phones;
};

bool valid(const string &phone)
{
    return phone.length() == 4;
}
string format(const string &phone)
{
    return string("(") + phone.substr(0, 1) + string(")") + phone.substr(1, phone.length());
}

void process(vector<PersonInfo> &people, ostream &os)
{
    for (const auto &entry : people)
    {
        ostringstream formatted, bad_phone_nums;
        for (const auto &phone : entry.phones)
        {
            if (!valid(phone))
            {
                bad_phone_nums << " " << phone;
            }
            else
            {
                formatted << " " << format(phone);
            }
        }
        if (bad_phone_nums.str().empty())
            os << entry.name << " " << formatted.str() << endl;
        else
            cerr << "input error for " << entry.name << " invalid numbers: " << bad_phone_nums.str() << endl;
    }
}

int main(int argc, char **argv)
{
    string line, word;
    vector<PersonInfo> people;

    ifstream input_file;
    if (argc == 2)
    {
        input_file.open(argv[1]);
        if (!input_file)
        {
            cerr << "unable to open file " << argv[1] << endl;
            return EXIT_FAILURE;
        }
    }
    istream &stream = input_file.is_open() ? static_cast<istream &>(input_file) : cin;
    while (getline(stream, line))
    {
        PersonInfo info;
        istringstream record(line);
        record >> info.name;
        while (record >> word)
            info.phones.push_back(word);
        people.push_back(info);
    }
    process(people, cout);

    return EXIT_SUCCESS;
}