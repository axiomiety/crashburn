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

class Message;

class Folder {
    public:
        void addMsg(const Message*);
        void remMsg(const Message*);
};

void Folder::addMsg(const Message*) {}
void Folder::remMsg(const Message*) {}

class Message {
    friend class Folder;
public:
    explicit Message(const string& str = "") : contents(str) { }
    Message(const Message&); // copy
    Message& operator=(const Message&); // copy-assignment
    ~Message();
    void save(Folder&);
    void remove(Folder&);
private:
    string contents;
    set<Folder*> folders;
    void add_to_folders(const Message&);
    void remove_from_folders();
};

void Message::save(Folder& f) {
    folders.insert(&f);
    f.addMsg(this);
}

void Message::remove(Folder& f) {
    folders.erase(&f);
    f.remMsg(this);
}

void Message::add_to_folders(const Message& m)
{
    for (auto f: m.folders)
        f->addMsg(this);
}

void Message::remove_from_folders()
{
    for (auto f : folders)
        f->remMsg(this);
    folders.clear();
}

Message::Message(const Message& m) : contents(m.contents), folders(m.folders)
{
    add_to_folders(m);
}

Message::~Message()
{
    remove_from_folders();
}

Message& Message::operator=(const Message& rhs)
{
    remove_from_folders();
    contents = rhs.contents;
    folders = rhs.folders;
    add_to_folders(rhs);
    return *this;
}

int main(int argc, char **argv)
{
    return EXIT_SUCCESS;
}