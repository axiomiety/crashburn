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
class Folder;

class Message
{
    friend class Folder;

public:
    explicit Message(const string &str = "") : contents(str) {}
    Message(const Message &);            // copy
    Message &operator=(const Message &); // copy-assignment
    ~Message();
    void save(Folder &);
    void remove(Folder &);
    friend void swap(Message &lhs, Message &rhs);
    ostream& print(ostream& out);
    static bool cmpMsg(const Message* a, const Message* b) { return a->contents < b->contents; };
private:
    string contents;
    set<Folder *> folders;
    void add_to_folders(const Message &);
    void remove_from_folders();
};

ostream& Message::print(ostream& out)
{
    return out << contents << endl;
}

class Folder
{
public:
    Folder(string s) : name(s) {}
    void addMsg(Message *);
    void remMsg(Message *);

private:
    string name;
    set<Message *, decltype(Message::cmpMsg)*> messages;
};

void Folder::addMsg(Message* msg) {
    messages.insert(msg);
}
void Folder::remMsg(Message* msg) {
    messages.erase(msg);
}

void Message::save(Folder &f)
{
    folders.insert(&f);
    f.addMsg(this);
}

void Message::remove(Folder &f)
{
    folders.erase(&f);
    f.remMsg(this);
}

void Message::add_to_folders(const Message &m)
{
    for (auto f : m.folders)
        f->addMsg(this);
}

void Message::remove_from_folders()
{
    for (auto f : folders)
        f->remMsg(this);
    folders.clear();
}

Message::Message(const Message &m) : contents(m.contents), folders(m.folders)
{
    add_to_folders(m);
}

Message::~Message()
{
    remove_from_folders();
}

Message &Message::operator=(const Message &rhs)
{
    remove_from_folders();
    contents = rhs.contents;
    folders = rhs.folders;
    add_to_folders(rhs);
    return *this;
}

void swap(Message &lhs, Message &rhs)
{
    using std::swap;
    for (auto f : lhs.folders)
        f->remMsg(&lhs);
    for (auto f : rhs.folders)
        f->remMsg(&rhs);
    swap(lhs.folders, rhs.folders);
    swap(lhs.contents, rhs.contents);

    for (auto f : lhs.folders)
        f->addMsg(&lhs);
    for (auto f : rhs.folders)
        f->addMsg(&rhs);
}

int main(int argc, char **argv)
{
    auto m1 = Message("message 1");
    auto m2 = Message("message 2");
    m1.print(cout);
    auto f1 = Folder("folder 1");
    auto f2 = Folder("folder 2");
    m1.save(f1);
    // m1.save(f2);

    return EXIT_SUCCESS;
}