<p align="center">
  <a href="https://github.com/mssola/dotfiles/actions?query=workflow%3Aci" title="CI status for the main branch"><img src="https://github.com/mssola/dotfiles/workflows/ci/badge.svg" alt="Build Status for main branch" /></a>
  <a href="http://www.gnu.org/licenses/gpl-3.0.txt" rel="nofollow"><img alt="License GPL 3" src="https://img.shields.io/badge/license-GPL_3-blue.svg" style="max-width:100%;"></a>
</p>

---

These are my personal dotfiles, use them at your own risk! You can download
everything by performing the following command (notice the `--recursive` flag):

```bash
$ git clone --recursive https://github.com/mssola/dotfiles.git
```

To install everything just run the `install.sh` script.

## GNU Emacs

I am using [Doom Emacs](https://github.com/hlissner/doom-emacs), see
[.doom.d/README.org](./.doom.d). For runtime dependencies just run:

```bash
$ doom doctor
```

## License

Every file is licensed under the GPLv3+ unless stated otherwise. I've tried to
mention every major influence on my `dotfiles`, but most surely I've forgotten
to add some people. If you feel like you should be mentioned somewhere in my
config files, please send me an email. You can find the full license in the
`LICENSE` file and below:

```
Copyright (C) 2014-2021 Miquel Sabaté Solà <mikisabate@gmail.com>

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
```
