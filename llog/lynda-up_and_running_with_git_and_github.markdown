---
layout: default
title: llog - lynda, up and running with git & github
category: pages
---

{% highlight bash %}
git init
git add
git status
git add . # everything - but is it recursive?

# set your config
git config --global user.name 'pie'
git config --global user.email 'a@abc.com'

# see!
git log # shows first line

git checkout <file> # takes it back from the staging environment

# when `git add`ing a file, it stores the version in a staging environment. Further edits on the same file will show in 2 places via `git status`

git reset HEAD index.html # removes the version in the staging environment - a.k.a. unstaging

# 'undelete'
git checkout index.html
# delete with the OS
# + add deleting to staging
git add index.html
# or
git rm index.html # automatically removes the file and pushes the deletion to your staging env

git add --all

~~ interlude
cat >> .git/info/exclude
*.swp
# for vim .swp files. Use ! to negate a pattern - like *.html followed by !foo.html - say if all html files were automatically generated except foo.html

# playground
git checkout <hash> # enough of hash so it's uniquely identified (4 chars min?)

git branch blah
git branch # master + HEAD detached from <hash>, * for current branch
# note that you stay in the current branch!
git checkout master

git branch alternate 5e2d

git merge app01 # from app01 into current branch

git branch -m app01 app1 # rename

git branch -D alternate # delete!

-- follow-up - git essential training with Kevin Skoglund

git clone <url.git> # local copy
git branch -a # shows all branches, you'll see HEAD -> origin/master
git checkout -b 02_01 origin/02_01 # get of branch on github in origin/02_01 into 02_01 and automatically switches to that branch
git checkout master # to revert to master branch

git clone --mirror <url.git> .git # clone the invisible git folder *only*
git config --bool core.bare false # from bare
git reset --hard # creates all the branches

# templating - create a special branch
git clone -b 00_start <url.git>

# remove .git folder
# follow with a `git init` to have it start from scratch
{% endhighlight %}
