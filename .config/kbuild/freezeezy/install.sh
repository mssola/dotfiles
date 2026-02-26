#!/usr/bin/env bash
# Copyright (C) 2026-Ω Miquel Sabaté Solà <mikisabate@gmail.com>
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

##
# Ensure you are root and dependencies are met.

if [ "$EUID" -ne 0 ];then
   echo "kbuild (error): run this as root."
   exit 1
fi

if [ ! -f /usr/bin/dracut ]; then
   echo "kbuild (error): you need 'dracut'."
   exit 1
fi

if [ ! -f /usr/sbin/btrfs ]; then
   echo "kbuild (error): you need 'btrfs'."
   exit 1
fi

##
# Before anything at all, create a snapshot.

btrfs subvolume snapshot / /.snapshots/pre-kernel-$name

##
# Copy files into filesystem and generate initramfs.

make modules_install
# NOTE: headers are not installed on purpose.
# Manual make install
cp arch/x86/boot/bzImage /boot/vmlinuz-$name
cp .config /boot/config-$name
cp System.map /boot/System.map-$name
dracut --kver=$name -f

##
# Update grub.

grub2-mkconfig -o /boot/grub2/grub.cfg
