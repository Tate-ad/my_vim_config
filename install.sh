#!/bin/bash

mkdir -p ~/.vimbackup

sudo apt-get install tmux -y
sudo apt-get install axel -y
sudo apt-get install vim vim-scripts vim-runtime vim-tiny vim-gnome -y
sudo apt-get install ttf-ancient-fonts -y
sudo apt-get install python-dev -y
sudo apt-get install python3-dev -y
sudo python get-pip.py
sudo pip install shadowsocks
sudo pip install httpie

sudo cp sslocal /etc/init/
sudo service sslocal start

ln -s bashrc ~/.bashrc
ln -s git-prompt.sh ~/.git-prompt.sh
ln -s vimrc ~/.vimrc
ln -s gitconfig ~/.gitconfig
