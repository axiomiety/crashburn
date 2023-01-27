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
    ostream &print(ostream &out);
    string contents;

private:
    set<Folder *> folders;
    void add_to_folders(const Message &);
    void remove_from_folders();
};

class Folder
{
public:
    Folder(string s) : name(s) {}
    void addMsg(Message *);
    void remMsg(Message *);
    string name;

private:
    set<Message *> messages;
};

ostream &Message::print(ostream &out)
{
    out << contents << " (folders: ";
    for (const auto f : folders)
        out << f->name << " ";

    return out << ")" << endl;
}

void Folder::addMsg(Message *msg)
{
    messages.insert(msg);
}
void Folder::remMsg(Message *msg)
{
    //auto num_removed = messages.erase(msg);
    //cout << "removed " << num_removed << " messages" << endl;
    if (messages.count(msg))
        messages.erase(msg);
    cout << messages.size() << endl;
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
    auto f1 = Folder("folder 1"), f2 = Folder("folder 2");
    auto m1 = Message("message 1"), m2 = Message("message 2");
        m1.save(f1);
        m1.save(f2);

        m1.print(cout);
        m2.print(cout);

    return EXIT_SUCCESS;
}