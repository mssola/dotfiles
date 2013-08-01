#!/bin/bash


# If something goes wrong, get the f*ck out.
set -e

# Install the files on this repo.
cp bashrc $HOME/.bashrc
cp irbrc $HOME/.irbrc
cp gemrc $HOME/.gemrc
cp gitcompletion.sh $HOME/.gitcompletion.sh
cp gitconfig $HOME/.gitconfig
cp global.gitignore $HOME/.gitignore
cp vimrc $HOME/.vimrc
cp hgrc $HOME/.hgrc
cp global.hgignore $HOME/.hgignore
cp rake_completion $HOME/.rake_completion
chmod +x $HOME/.rake_completion

# Download and install the review utility.
cd /tmp
git clone https://github.com/mssola/review
cd review
sudo make install
cp misc/review_completion.sh $HOME/.review_completion.sh

# Download and install the g utility
cd /tmp
git clone https://github.com/mssola/g
cd g
cp g.sh $HOME/.g.sh
cp gcompletion.sh $HOME/.gcompletion.sh
