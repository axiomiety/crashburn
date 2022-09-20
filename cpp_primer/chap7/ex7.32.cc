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
using std::ostream;
using std::string;
using std::vector;

class Screen;

class Window_mgr
{
    using ScreenIndex = vector<Screen>::size_type;

public:
    Window_mgr();
    void clear(ScreenIndex);

private:
    vector<Screen> screens;
};

class Screen
{
    friend void Window_mgr::clear(ScreenIndex);

public:
    typedef std::string::size_type pos;
    Screen() = default;
    Screen(pos ht, pos wd, char c) : height(ht), width(wd), contents(ht * wd, c) {}
    Screen(pos ht, pos wd) : Screen(ht, wd, 0) {}
    char get() const { return contents[cursor]; }
    inline char get(pos ht, pos wd) const;
    Screen &set(char c);
    Screen &set(pos ht, pos wd, char c);
    Screen &move(pos r, pos c);
    Screen &display(ostream &os)
    {
        do_display(os);
        return *this;
    }
    const Screen &display(ostream &os) const
    {
        do_display(os);
        return *this;
    }

private:
    pos cursor = 0;
    pos height = 0, width = 0;
    string contents;
    void do_display(ostream &os) const { os << contents; };
};

inline Screen &Screen::move(pos r, pos c)
{
    pos row = r * width;
    cursor = row + c;
    return *this;
}
char Screen::get(pos r, pos c) const
{
    pos row = r * width;
    return contents[row + c];
}
inline Screen &Screen::set(char c)
{
    contents[cursor] = c;
    return *this;
}
inline Screen &Screen::set(pos r, pos col, char c)
{
    contents[r * width + col] = c;
    return *this;
}

Window_mgr::Window_mgr()
{
    screens = vector<Screen>{Screen(24, 80, ' ')};
}
void Window_mgr::clear(ScreenIndex i)
{
    Screen &s = screens[i];
    s.contents = string(s.height * s.width, ' ');
}

int main(int argc, char **argv)
{
    Screen mys(5, 5, 'X');
    mys.move(4, 0).set('#').display(cout);
    cout << "\n";
    mys.display(cout);
    cout << "\n";
    return EXIT_SUCCESS;
}