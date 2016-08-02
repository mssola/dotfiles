## GNU Emacs Modules written by Miquel Sabaté Solà

This directory contains Lisp modules I've written.

### g.el

A port of my [g](https://github.com/mssola/g) script for GNU Emacs. This is not
helpful all the time, but it does help whenever Projectile does not cover me.
This module implements the following interactive functions:

- `g`: allows users to jump through a shortcut. After performing the jump, the
  user will be presented with the `dired` mode.
- `g-add`: add a new shortcut that points to a valid path.
- `g-remove`: remove an existing shortcut.
- `g-list`: list all the available shortcuts.

This module is completely compatible with the `g.sh` script, which means that
they both can be run side by side without any problems.
