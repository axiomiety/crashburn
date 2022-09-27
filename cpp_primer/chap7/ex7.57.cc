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
using std::istream;
using std::map;
using std::ostream;
using std::string;
using std::vector;

class Account
{
public:
    void calculate() { amount += amount * interestRate; }
    static double rate() { return interestRate; }
    static void rate(double);

private:
    string owner;
    double amount;
    static double interestRate;
    static double initRate();
    static constexpr int period = 30;
    double daily_tbl[period];
};

void Account::rate(double newRate)
{
    interestRate = newRate;
}

double Account::initRate() { return 1.6; };

double Account::interestRate = initRate();

int main(int argc, char **argv)
{
    Account ac1;
    Account *ac2 = &ac1;
    auto r = ac1.rate();
    cout << r << endl;
    r = ac2->rate();
    cout << r << endl;
    return EXIT_SUCCESS;
}