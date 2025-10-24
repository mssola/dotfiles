#!/usr/bin/env bash
# Copyright (C) 2025-Ω Miquel Sabaté Solà <mikisabate@gmail.com>
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

set -ex

if [ "$EUID" -ne 0 ];then
   echo "kbuild (error): run this as root."
   exit 1
fi

if [ ! -f files.txt ]; then
   echo "kbuild (error): there must be a 'files.txt' file."
   exit 1
fi

xargs rm -f < files.txt
rm -rf /usr/lib/modules/$name
rm -rf /boot/dtbs/$name
rm -rf /usr/lib/firmware/$name
rm -f /boot/initrd-$name
rm -f /boot/initrd.img-$name

if [ -z "$EDITOR" ]; then
   EDITOR=vi
fi

# Edit the configuration file we just produced just in case and update it.
$EDITOR /etc/grub.d/40_custom
grub2-mkconfig -o /boot/grub2/grub.cfg
