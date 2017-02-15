#!/bin/bash

PROJECTS="$HOME/Projects"

# If something goes wrong, get the f*ck out.
set -e

# Initialize and update git submodules.
git submodule init
git submodule update

# Install the files on this repo.
cp bashrc $HOME/.bashrc
cp bash_profile $HOME/.bash_profile
cp psqlrc $HOME/.psqlrc
cp valgrindrc $HOME/.valgrindrc
cp irbrc $HOME/.irbrc
cp gemrc $HOME/.gemrc
cp gitcompletion.sh $HOME/.gitcompletion.sh
cp gitconfig $HOME/.gitconfig
cp global.gitignore $HOME/.gitignore
cp agignore $HOME/.agignore
cp hgrc $HOME/.hgrc
cp hgcompletion.sh $HOME/.hgcompletion.sh
cp global.hgignore $HOME/.hgignore
cp tmux.conf $HOME/.tmux.conf
cp rake_completion $HOME/.rake_completion
cp rvmrc $HOME/.rvmrc
cp gnupg/gpg.conf $HOME/.gnupg/gpg.conf
cp gnupg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
cp mbsyncrc $HOME/.mbsyncrc
chmod +x $HOME/.rake_completion

# Some random scripts
cp -r bin $HOME/bin
rm -rf $HOME/bin/test

# i3 and X stuff. An incomplete list of openSUSE packages that I need in order
# to have a good i3 environment is: i3 xlockmore dunst i3status nitrogen scrot
# breeze5-cursors konsole gpicview okular dmenu the_silver_searcher.
mkdir -p $HOME/.config/dunst $HOME/.i3
cp i3/config $HOME/.i3/config
cp i3status.conf $HOME/.i3status.conf
cp inputrc $HOME/.inputrc
cp Xresources $HOME/.Xresources
cp -r fonts $HOME/.fonts
cp config/dunst/dunstrc $HOME/.config/dunst/dunstrc

# Make Caps Lock an extra control.
# sudo localectl set-x11-keymap es "" cat, 'ctrl:nocaps'
sudo localectl set-x11-keymap us "" us, 'ctrl:nocaps'

# Wipe out the current vim config and replace it with this one.
rm -rf $HOME/.vim
rm -rf $HOME/.vimrc
cp -rf vim $HOME/.vim
cp vimrc $HOME/.vimrc
vim +PluginInstall +qall

# GNU Emacs
rm -rf $HOME/.emacs.d
cp emacs/custom.el $HOME/.emacs.d/custom.el
cp emacs/init.el $HOME/.emacs.d/init.el
ln -s emacs/init.org $HOME/.emacs.d/init.org

# Update the style for KTE users (KWrite, Kate, KDevelop & Kile). Note that
# there are two configs: the global one and the KDE one. The KDE one assumes
# that all your KDE projects are stored in the `$HOME/Projects/kde` directory.
# The global one is installed in the $HOME directory.
mkdir -p $PROJECTS/kde
cp kte/kde.kateconfig $PROJECTS/kde/.kateconfig
cp kte/global.kateconfig $HOME/.kateconfig

# Download and install the g utility
cd /tmp
rm -rf g
git clone https://github.com/mssola/g
cd g
cp g.sh $HOME/.g.sh
cp gcompletion.sh $HOME/.gcompletion.sh

# Download and install the review utility
cat <<HERE

We'll now proceed to install the Review utility. The Review utility will be
installed in /opt/review, that's why we ask for root permissions.

HERE

cd /tmp
rm -rf review
git clone https://github.com/mssola/review
cd review
./install.sh

# Install the completion file for the `docker` command.
wget https://raw.githubusercontent.com/docker/docker/master/contrib/completion/bash/docker -O $HOME/.dockercompletion.sh
