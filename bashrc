##
# Locale.

# Set LC_ALL always to UTF-8.
export LC_ALL=en_US.UTF-8

##
# Basics.

# Beautifying ls command.
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# Alias related to cd
alias ..='cd ..'
alias cd..='cd ..'

##
# Utility functions.

# Sources the given file if it really exists.
source_maybe() {
    if [ -f $1 ]; then
        source $1
    fi
}

##
# Editors & terminals.

# Set vim as the default editor.
export EDITOR=emacsclient-a-nw
export VISUAL=emacsclient-a-nw
alias vi=vim
alias e=$EDITOR

# Shortcut for GNU Emacs.
em() {
    emacsclient -c -a "" -t "$1"
}

# Alias xdg-open to just xo
alias xo=xdg-open

# We want a full-fledged 256-color terminal.
TERM=xterm-256color

# Set the LESS and the PAGER environment variables.
export LESS="FSRX"
export PAGER=less

# git thingies.
source_maybe $HOME/.gitcompletion.sh
alias gti=git

# hg thingies.
source_maybe $HOME/.hgcompletion.sh
__hg_branch() {
  if [ -d .hg ]; then
    local b=`cat .hg/branch`
    echo -e "\033[0;32m@\033[1;32m$b\033[0m"
  fi
}

# Setting up PS1.
PS1='\u:\w$(__hg_branch)$(__git_ps1 "\[\033[0;32m\]@%s\[\033[0m\]\]") $ '

##
# Programming languages and environments.

# Rake completion
if [ -f $HOME/.rake_completion ]; then
    complete -C $HOME/.rake_completion -o default rake
fi

# Alias bundle exec
alias be='bundle exec'

# After experimenting with GCC & Clang, I'm sticking with GCC.
export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

# Go things
export GOROOT_BOOTSTRAP=/opt/go
export GOROOT=$HOME/Projects/go
export GOPATH=$HOME/Projects/golang
export PATH=$GOROOT/bin:$PATH:$GOPATH/bin

##
# Misc.

# The g utility. See: https://github.com/mssola/g
source_maybe $HOME/.g.sh
source_maybe $HOME/.gcompletion.sh

# Complete the `docker` command if possible.
source_maybe $HOME/.dockercompletion.sh

# Add the `bin` dir to the path if possible. See:
# https://github.com/mssola/dotfiles.
if [ -d $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
fi

# Some packages install inside of ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Some other misc. alias.
alias iosc="osc -A https://api.suse.de"
alias ag='ag --nocolor --path-to-ignore ~/.agignore'
alias random_string="cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1"

# Cargo environment.
source_maybe $HOME/.cargo/env

# SUSE
export SALT_DIR="$HOME/Projects/kubic-project/salt"
export SALT_PATH="$HOME/Projects/kubic-project/salt"

export NODE_VERSION="8"

# Finally RVM requires it to be the last thing on the PATH for whatever reason.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin"

##
# Lastly I had some problems with the GPG agent recently. So I copied a solution
# from https://github.com/jessfraz/dotfiles

# Use a tty for gpg
GPG_TTY=$(tty)
export GPG_TTY

# Start the gpg-agent if not already running
if ! pgrep -x -u "${USER}" gpg-agent >/dev/null 2>&1; then
  gpg-connect-agent /bye >/dev/null 2>&1
  gpg-connect-agent updatestartuptty /bye >/dev/null
fi

# Set SSH to use gpg-agent
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
fi

# Add alias for ssh to update the tty
alias ssh="gpg-connect-agent updatestartuptty /bye >/dev/null; ssh"

# Handy alias for the ip command
alias ip="ip --color"
alias ipb="ip --color --brief"