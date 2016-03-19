git config --global user.email 'axiomiety@gmail.com'
git config --global user.name 'axiomiety'
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'
git config --global core.excludesfile ~/.gitignore
# the repos would have been cloned via puppet
#cd ~
#git clone https://github.com/axiomiety/setup.git
#git clone https://github.com/axiomiety/otw.git
cd ~/setup
git remote add setup https://github.com/axiomiety/setup.git
#cd ~/otw
#git remote add otw https://github.com/axiomiety/otw.git
cd ~/crashburn
git remote add crashburn https://github.com/axiomiety/crashburn.git
cd ~/axiomiety.github.io
git remote add axiomiety.github.io https://github.com/axiomiety/axiomiety.github.io.git
cd ~/exercism-io
git remote add exercism-io https://github.com/axiomiety/exercism-io.git

git config --global core.editor "vim"
