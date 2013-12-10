#!/bin/bash


# If something goes wrong, get the f*ck out.
set -e

# Install the files on this repo.
cp bashrc $HOME/.bashrc
cp psqlrc $HOME/.psqlrc
cp irbrc $HOME/.irbrc
cp gemrc $HOME/.gemrc
cp gitcompletion.sh $HOME/.gitcompletion.sh
cp gitconfig $HOME/.gitconfig
cp global.gitignore $HOME/.gitignore
cp hgrc $HOME/.hgrc
cp hgcompletion.sh $HOME/.hgcompletion.sh
cp global.hgignore $HOME/.hgignore
cp rake_completion $HOME/.rake_completion
chmod +x $HOME/.rake_completion

# Wipe out the current vim config and replace it with this one.
rm -rf $HOME/.vim
rm -rf $HOME/.vimrc
cp -rf vim $HOME/.vim
cp vimrc $HOME/.vimrc

# Download and install the review utility.
cd /tmp
git clone https://github.com/mssola/review
cd review
perl install.pl
cp misc/review_completion.sh $HOME/.review_completion.sh

# Download and install the g utility
cd /tmp
git clone https://github.com/mssola/g
cd g
cp g.sh $HOME/.g.sh
cp gcompletion.sh $HOME/.gcompletion.sh
