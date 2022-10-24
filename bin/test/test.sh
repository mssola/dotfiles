#!/bin/sh
# Copyright (C) 2014-2022 Miquel Sabaté Solà <mikisabate@gmail.com>
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

# Back up the current files and get paths straight.
set -e
dir="$( cd "$( dirname "$0" )" && pwd )"
license=$(readlink -f "$dir/../license")
cp "$dir/main.cpp" "$dir/main.cpp.old"
cp "$dir/README.md" "$dir/README.md.old"
set +e
status=0

# license main.cpp
$license "$dir/main.cpp"
d=$(diff "$dir/main.cpp" "$dir/main.cpp.expected")
if [ -n "$d" ]; then
    echo "$d"
    status=1
fi

# license README.md
$license "$dir/README.md"
d=$(diff "$dir/README.md" "$dir/README.md.expected")
if [ -n "$d" ]; then
    echo "$d"
    status=1
fi

# Tear down
mv "$dir/main.cpp.old" "$dir/main.cpp"
mv "$dir/README.md.old" "$dir/README.md"
if [ "$status" -eq "1" ]; then
    echo "FAIL!"
else
    echo "OK"
fi
exit $status
