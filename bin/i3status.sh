#!/bin/bash
# Copyright (C) 2020-2022 Miquel Sabaté Solà <mikisabate@gmail.com>
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

# This script was written by someone else and I am not sure who is the original
# author (it exists multiple versions of it). I have not done any modifications
# to it.
#
# This script simple prepends an indicator with the language keymap.

i3status --config ~/.i3status.conf | while :
do
    read line
    LG=$(setxkbmap -query | awk '/layout/{print $2}')

    if [ "$LG" = "es" ]; then
        LG="ca"
    fi
    echo "LG: $LG | $line" || exit 1
done
