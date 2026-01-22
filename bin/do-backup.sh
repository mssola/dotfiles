#!/usr/bin/env bash
# Copyright (C) 2025-Ω Miquel Sabaté Solà <mssola@mssola.com>
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
# Advertise the start of this script.

if command -v notify-send &> /dev/null; then
    notify-send "do-backup" "Starting to backup!"
fi

##
# Set up environment.

backup_name="backup-$(date +%Y-%m-%d)"
backup_base_dir="$HOME/data"
backup_dir="$backup_base_dir/$backup_name"
mkdir $backup_dir

##
# Email.

mkdir -p "$backup_dir/mail"
mail_stores=("ajuntament" "comsuse" "desuse" "mailbox" "gmail" "sindicat" "urv" "uoc")
for store in "${mail_stores[@]}"; do
    store_dir="$HOME/data/mail/$store"

    echo "do-backup (info): Backing up email ($store)"
    cp -r $store_dir "$backup_dir/mail/$store"
done

##
# Organization and documents.

echo "do-backup (info): Backing up 'org'."
cp -r ~/org "$backup_dir/"

echo "do-backup (info): Backing up 'doc'."
cp -r ~/doc "$backup_dir/"

##
# Applications.

echo "do-backup (info): apps."
cp -r ~/.config/mihi "$backup_dir/"

##
# Compress things, remove the old directory and announce things.

tar czf "$backup_base_dir/$backup_name.tar.gz" -C "$backup_base_dir/" "$backup_name"
rm -r $backup_dir

if command -v notify-send &> /dev/null; then
    notify-send "do-backup" "Done!"
fi
