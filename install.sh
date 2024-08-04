#!/bin/bash
# Copyright (C) 2014-2023 Miquel Sabaté Solà <mikisabate@gmail.com>
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
# Let's ask for some dependencies and nice-to-have binaries.

binaries=(git curl vim emacs)
for bin in "${binaries[@]}"; do
    if ! [ -x "$(command -v ${bin})" ]; then
        echo "Error: the binary '${bin}' is not installed." >&2
        exit 1
    fi
done

##
# Main procedure: link as many files as possible.

ary=(.git .gitignore .gitmodules LICENSE *.gitignore *.yml *.org install.sh
    update.sh .emacs.d .config .gnupg Images .i3 .i3status.conf README.md)
ignore="-name ${ary[0]}"
for i in "${ary[@]:1:${#ary[@]}}"; do
    ignore+=" -or -name $i"
done

# shellcheck disable=SC2086
# shellcheck disable=SC2044
for file in $(find "$(pwd)" -mindepth 1 -maxdepth 1 ! \( $ignore \)); do
    # The -F and -f flags of ln suck. Let's rm all the way.
    rm -rf "${HOME:?}/$(basename $file)"
    ln -s "$(readlink -f $file)" "${HOME:?}/$(basename $file)"
done

##
# Some binaries are good to be installed globally.

for i in bin/zypper-* e; do
    if [ ! -f "/usr/bin/$(basename $i)" ]; then
        sudo ln -s "$(readlink -f $i)" "/usr/bin/$(basename $i)"
    fi
done

##
# And now let's go for the exceptional cases.

# Link the global.gitignore file
rm -rf "${HOME:?}/.gitignore"
ln -s "$(readlink -f .global.gitignore)" "${HOME:?}/.gitignore"

# Copy entries of some of the directories inside of maybe existing ones.
for i in .config .gnupg; do
    mkdir -p "${HOME:?}/$i"
    cp -r "$i"/* "${HOME:?}/$i/"
done

##
# GNU Emacs: just install Doom and take it from there.

rm -rf "${HOME:?}/.config/doom"
ln -s "$(readlink -f .config/doom)" "${HOME:?}/.config/doom"

if [ -d "${HOME:?}/.config/emacs" ]; then
    if [ -x "$(command -v doom)" ]; then
        doom upgrade
    fi
else
    git clone https://github.com/hlissner/doom-emacs "${HOME:?}/.config/emacs"
fi

"${HOME:?}/.config/emacs/bin/doom" install

##
# Other stuff

# Get git completiom right.
if ! [ -f "${HOME:?}/.git-prompt.sh" ]; then
    curl -o "${HOME:?}/.git-prompt.sh" https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
fi

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
if [ -e "${HOME:?}/src/github.com/mssola/soria/soria-theme.el" ]; then
    echo "You are all set. Enjoy!"
else
    echo "Note: you still need to install https://github.com/mssola/soria.git"
fi
