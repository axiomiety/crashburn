git config --global user.email 'axiomiety@gmail.com'
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'
git config --global core.excludesfile ~/.gitignore
cd ~
git clone https://github.com/axiomiety/setup.git
git clone https://github.com/axiomiety/git.git
cd setup
git remote add setup https://github.com/axiomiety/setup.git
cd ../otw
git remote add otw https://github.com/axiomiety/otw.git

git config --global core.editor "vim"
