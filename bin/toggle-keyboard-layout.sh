#!/bin/bash
# Copyright (C) 2016-2021 Miquel Sabaté Solà <mikisabate@gmail.com>
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

# Idea taken from: https://github.com/ereslibre/dotfiles. All the credit goes
# to him.

layout=$(setxkbmap -query | awk '/layout/{print $2}')
if [ "$layout" == 'mi' ]
then
    setxkbmap es -option ctrl:nocap
else
    setxkbmap mi -option ctrl:nocap
fi
killall -SIGUSR1 i3status
