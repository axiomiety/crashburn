git config --global user.email 'axiomiety@gmail.com'
git config --global user.name 'axiomiety'
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'
git config --global core.excludesfile ~/.gitignore
# the repos would have been cloned via puppet
#cd ~
#git clone https://github.com/axiomiety/setup.git
#git clone https://github.com/axiomiety/otw.git
export REPO_DIR=/shared
cd ${REPO_DIR}/setup
git remote add setup https://github.com/axiomiety/setup.git
#cd ${REPO_DIR}/otw
#git remote add otw https://github.com/axiomiety/otw.git
cd ${REPO_DIR}/crashburn
git remote add crashburn https://github.com/axiomiety/crashburn.git
cd ${REPO_DIR}/axiomiety.github.io
git remote add axiomiety.github.io https://github.com/axiomiety/axiomiety.github.io.git
cd ${REPO_DIR}/exercism-io
git remote add exercism-io https://github.com/axiomiety/exercism-io.git

git config --global core.editor "vim"
