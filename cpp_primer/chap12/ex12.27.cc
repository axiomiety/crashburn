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
#include <format>

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
        QueryResult(string token, shared_ptr<vector<string>> lines, shared_ptr<set<line_no>> lineIndexes) : token(token), lines(lines), lineIndexes(lineIndexes) {};
        string toString();
    //private:
        string token;
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
            lineIndexes->insert(lines->size()-1);
        }
    }

}

QueryResult TextQuery::query(string word) 
{
    static shared_ptr<set<line_no>> nodata(new set<line_no>);
    if (wordToLines.count(word) == 0)
        return QueryResult(word, shared_ptr<vector<string>>(lines), nodata);
    return QueryResult(word, shared_ptr<vector<string>>(lines), shared_ptr<set<line_no>>(wordToLines[word]));
}

string QueryResult::toString()
{
    ostringstream out;
    out << token << " was found across " << lineIndexes->size() << " line(s)\n";
    for (auto lineIdx: *lineIndexes)
        out << "(line " << lineIdx+1 << ") " << lines->at(lineIdx) << endl;
    return out.str();
}

ostream& print(ostream& out, QueryResult qr)
{
    out << qr.toString() << endl;
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