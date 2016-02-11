#!/bin/bash
SETUP_DIR=/home/vagrant/setup # this gets run as root : (
cp -v ${SETUP_DIR}/.vimrc ~/
cp -v ${SETUP_DIR}/.tmux.conf ~/
cp -v ${SETUP_DIR}/.inputrc ~/
cp -v ${SETUP_DIR}/.bashrc ~/
