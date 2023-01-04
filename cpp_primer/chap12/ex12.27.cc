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

using line_no = vector<string>::size_type;
class QueryResult;

class TextQuery
{
    public:
        TextQuery(ifstream&);
        QueryResult query(string);

    private:
        shared_ptr<vector<string>> lines;
        map<string, shared_ptr<set<line_no>>> wordToLines;
};

class QueryResult
{
    public:
        QueryResult() = default;
        QueryResult(shared_ptr<vector<string>> lines, shared_ptr<set<line_no>> lineIndexes) : lines(lines), lineIndexes(lineIndexes) {};

    private:
        shared_ptr<vector<string>> lines;
        shared_ptr<set<line_no>> lineIndexes;
};

TextQuery::TextQuery(ifstream& in) : lines(new vector<string>)
{
    string line, token;
    while (getline(in, line))
    {
        lines->push_back(line);
        istringstream is(line);
        while (is >> token)
        {
            auto& lineIndexes = wordToLines[token];
            if (!lineIndexes)
                lineIndexes = make_shared<set<line_no>>();
            lineIndexes->insert(lines->size());
        }
    }

}

QueryResult TextQuery::query(string word) 
{
    if (wordToLines.count(word) == 0)
        return QueryResult();
    
    return QueryResult(shared_ptr<vector<string>>(lines), shared_ptr<set<line_no>>(wordToLines[word]));
}

ostream& print(ostream& out, QueryResult qr)
{
    out << "foobar";
    return out;
}

void runQueries(ifstream& infile)
{
    TextQuery tq(infile);
    cout << "done processing file" << endl;
    while (true)
    {
        cout << "enter a word to look for (q to quit): ";
        string s;
        if (!(cin >> s) || s == "q")
            break;
        print(cout, tq.query(s)) << endl;
    }
}

int main(int argc, char **argv)
{
    ifstream in(argv[1]);
    runQueries(in);
}