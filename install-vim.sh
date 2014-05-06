#!/bin/bash

# Use this script to compile vim from the mercurial repo. You should only be
# doing this if you *really* know what you're doing. You can configure the base
# path of the vim source code with the following variable:
BASE=/opt


# Move to $BASE and fetch vim.
cd $BASE
if [ -d "vim" ]; then
  hg pull
  hg update
else
  hg clone https://vim.googlecode.com/hg/ vim
fi

# Remove the previous config.
cd vim
rm -f src/auto/config.cache

# I configure vim with the same options as Archlinux with the following
# differences:
#
#   - I don't compile support for the Perl interpreter.
#   - I enable X support (so we have clipboard support).
#
# This way, vim is as tiny as possible while keeping the features I need.
./configure \
  --prefix=/usr \
  --localstatedir=/var/lib/vim \
  --with-features=huge \
  --enable-gpm \
  --enable-acl \
  --disable-gui \
  --enable-multibyte \
  --enable-cscope \
  --disable-netbeans \
  --disable-perlinterp \
  --disable-pythoninterp \
  --disable-python3interp \
  --disable-rubyinterp \
  --disable-luainterp

# Make and make install.
make && make install

