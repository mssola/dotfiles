# dotfiles

To automatically install everything, just execute the `install.sh` script.
Take a look at this script since it might perform some actions that are
not of your satisfaction. Anyways, by default this script takes care of
initializing and updating submodules (in my case, I keep the vundle repo as
a submodule). Vim plugins will also get installed automatically.

And that's about it. Use this at your own risk ;)

## GNU Emacs

For the configuration of Emacs I'm trying out using symlinks instead of copying
files into the right places. For this reason, for now I'm not providing an
automatical way to install everything (this is left to be done in the near
future). The `emacs/init.el` and `emacs/gtkrc` files should be symlinked into
`~/.emacs.d`.

I run GNU Emacs in daemon mode, but I don't use systemd for that. Instead, in
the `i3/config` file you'll see that I'm using `-a ""` as an argument of
`emacsclient`. This instructs `emacsclient` to spawn a new server if there
isn't one already. This means that the first time around it will take some
time, but in the next time everything will be alright.

Last but not least, I'm using my customized `soria` color theme. This theme is
unfortunately not available in any way other than Github. Therefore, you should
clone it yourself in https://github.com/mssola/soria, and add it into
the `~/.emacs.d` directory.

## Runtime dependencies

There are some runtime dependencies that matter to either Vim or Emacs. In
openSUSE (probably similar in your distribution) they can be solved like this:

```
$ go get -u golang.org/x/tools/cmd/goimports
$ go get -u github.com/rogpeppe/godef
$ go get -u github.com/dougm/goflymake
$ go get -u github.com/nsf/gocode
$ sudo zypper in spell
```

Also, in Emacs the markdown mode requires a markdown-to-HTML converter. There
are multiple options, but for me `calibre` seems to work just fine.

## Weechat certificates

For connecting to OFTC I'm using my own SSL certificates. OFTC has good
documentation on how to do this [here](https://www.oftc.net/NickServ/CertFP/).

## License

Everything except the `bin` directory and any Emacs Lisp file is licensed under
the MIT/X11 license, since these files are mere configuration files that I want
to keep simple in regards to licensing. You can find the full license in the
`LICENSE` file and below:

```
Copyright (c) 2011-2016 Miquel Sabaté Solà

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```

The files under the `bin` directory and all Emacs Lisp files are licensed under
the GPLv3 (or at your option any later version). The license itself can be
found in the `LICENSE.gpl3` file, and the header of it is as follows:

```
Copyright (C) 2014-2016 Miquel Sabaté Solà <mikisabate@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```
