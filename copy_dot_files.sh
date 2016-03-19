#!/bin/bash
HOME_DIR=/home/vagrant
SETUP_DIR=/home/vagrant/setup # this gets run as root : (
cp -v ${SETUP_DIR}/.vimrc ${HOME_DIR}
cp -v ${SETUP_DIR}/.tmux.conf ${HOME_DIR}
cp -v ${SETUP_DIR}/.inputrc ${HOME_DIR}
cp -v ${SETUP_DIR}/.bashrc ${HOME_DIR}

