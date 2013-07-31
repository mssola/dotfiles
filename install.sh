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
cp rake_completion $HOME/.rake_completion
chmod +x $HOME/.rake_completion
