#!/bin/bash
# Copyright (C) 2020 Miquel Sabaté Solà <mikisabate@gmail.com>
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

icon="$HOME/doc/imatges/lock.png"
tmpbg='/tmp/screen.png'

# Take a screenshot, then convert do fancy stuff to make it fuzzy, and then set
# the lock icon at the center

scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"

convert "$tmpbg" "$icon" -geometry +650+300 -composite -matte "$tmpbg"
# convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg" # NOTE: On the laptop

i3lock -e -n -i "$tmpbg"

# Turn the screen of after some delay.
sleep 60; pgrep i3lock && xset dpms force off
