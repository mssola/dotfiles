#!/bin/bash
# Copyright (C) 2014-2019 Miquel Sabaté Solà <mikisabate@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

set -e

##
# First some checks: we need GNU Emacs and Vim because we are going to
# initialize them right after installing their files.

if ! [ -x "$(command -v emacs)" ]; then
  echo 'Error: GNU Emacs is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v vim)" ]; then
  echo 'Error: Vim is not installed.' >&2
  exit 1
fi

##
# Main procedure: link as many files as possible.

ary=(.git .gitignore .gitmodules LICENSE *.gitignore *.yml *.org install.sh
     update.sh .emacs.d .config .gnupg Images)
ignore="-name ${ary[0]}"
for i in "${ary[@]:1:${#ary[@]}}"; do
    ignore+=" -or -name $i"
done

# shellcheck disable=SC2086
# shellcheck disable=SC2044
for file in $(find "$(pwd)" -mindepth 1 -maxdepth 1 ! \( $ignore \)); do
    # The -F and -f flags of ln suck. Let's rm all the way.
    rm -rf "${HOME:?}/$(basename $file)"
    ln -s "$file" "${HOME:?}/$(basename $file)"
done

##
# And now let's go for the exceptional cases.

# Link the global.gitignore file
rm "${HOME:?}/.gitignore"
ln -s global.gitignore "${HOME:?}/.gitignore"

# Copy entries of some of the directories inside of maybe existing ones.
for i in Images .config .gnupg; do
    mkdir -p "${HOME:?}/$i"
    cp -r "$i"/* "${HOME:?}/$i/"
done

# GNU Emacs: most files can be simply linked, the only exception being
# init.el. To know why check .emacs.d/README.org.
mkdir -p "${HOME:?}/.emacs.d"
for i in lisp org-templates snippets custom.el gtkrc init.org; do
    rm -rf "${HOME:?}/.emacs.d/$i"
    ln -s "$(readlink -f .emacs.d/$i)" "${HOME:?}/.emacs.d/$i"
done
rm -f "${HOME:?}/.emacs.d/init.elc"
cp .emacs.d/init.el "${HOME:?}/.emacs.d/init.el"
emacs -l "${HOME:?}/.emacs.d/init.el" --eval "(kill-emacs)"

##
# Other stuff

# Initialize Vim plugins.
vim +PluginInstall +qall

# Download and install the g utility
pushd /tmp
rm -rf g
git clone https://github.com/mssola/g
cd g
cp g.sh "${HOME:?}/.g.sh"
cp gcompletion.sh "${HOME:?}/.gcompletion.sh"
popd

echo ""
echo "Note: you still need to install https://github.com/mssola/soria.git"
