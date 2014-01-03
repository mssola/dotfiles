#!/bin/bash


# If something goes wrong, get the f*ck out.
set -e

# Initialize and update git submodules.
git submodule init
git submodule update

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
cat <<HERE
You have successfully installed my vim configuration files. However, you have
to type the following command inside vim to install all the plugins that I'm
using:

  :BundleInstall

HERE

# Download and install the review utility.
cd /tmp
rm -rf review
git clone https://github.com/mssola/review
cd review
perl install.pl
cp misc/review_completion.sh $HOME/.review_completion.sh

# Download and install the g utility
cd /tmp
rm -rf g
git clone https://github.com/mssola/g
cd g
cp g.sh $HOME/.g.sh
cp gcompletion.sh $HOME/.gcompletion.sh
