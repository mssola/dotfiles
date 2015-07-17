# Set LC_ALL always to UTF-8
export LC_ALL=en_US.UTF-8

# beautifying ls command
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# Alias related to cd
alias ..='cd ..'
alias cd..='cd ..'

# Set vim as the default editor
export EDITOR=vim
export VISUAL=vim
alias vi=vim

# We want a full-fledged 256-color terminal.
TERM=xterm-256color

# Set the LESS and the PAGER environment variables.
export LESS="FSRX"
export PAGER=less

# git thingies.
source $HOME/.gitcompletion.sh
alias gti=git

# hg thingies.
source $HOME/.hgcompletion.sh
__hg_branch() {
  if [ -d .hg ]; then
    local b=`cat .hg/branch`
    echo -e "\033[0;32m@\033[1;32m$b\033[0m"
  fi
}

# Setting up PS1.
PS1='\u:\w$(__hg_branch)$(__git_ps1 "\[\033[0;32m\]@\[\033[1;32m\]%s\[\033[0m\]\]") $ '

# Rake completion
complete -C $HOME/.rake_completion -o default rake

# KDE Paths
export KDEDIR=$HOME/.kde
export KDEDIRS=$KDEDIR
export KF5_SRC=$HOME/Projects/kde/kf5

# Export the standard paths to include KDE
export PATH=/usr/local/heroku/bin:$PATH:$KDEDIR/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$KDEDIR/lib
export XDG_DATA_DIRS=$XDG_DATA_DIRS:$KDEDIR/share

# Export the CMake paths so it searches for KDE in the right places
export CMAKE_PREFIX_PATH=$KDEDIR:$CMAKE_PREFIX_PATH
export CMAKE_LIBRARY_PATH=$KDEDIR/lib:$CMAKE_LIBRARY_PATH
export CMAKE_INCLUDE_PATH=$KDEDIR/include:$CMAKE_INCLUDE_PATH

# Function to whip the KDevelop cache
alias wduchain='rm -rf $HOME/.cache/kdevduchain/*'

# Let's change to KF5 mode.
alias frameworks='source $KF5_SRC/kf5.sh; eval `dbus-launch`; kdeinit5; echo'

# Alias bundle exec
alias be='bundle exec'

# Set the CC and CXX variables to clang, but allow a fallback shortcut, since
# some projects do not support clang yet (e.g. HHVM).
alias GCC='CC=/usr/bin/gcc CXX=/usr/bin/g++'
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# Go things
export GOROOT_BOOTSTRAP=/opt/go1.4
export GOROOT=$HOME/Projects/go
export GOPATH=$HOME/Projects/golang
export PATH=$HOME/Projects/golang/bin:$HOME/Projects/go/bin:$PATH

# The Review utility. See: https://github.com/mssola/review
export PATH=/opt/review:$PATH
source $HOME/.review_completion.sh

# The g utility. See: https://github.com/mssola/g
source $HOME/.g.sh
source $HOME/.gcompletion.sh

# The td utility. See: https://github.com/mssola/td
if [ -f $HOME/.tdcompletion.sh ]; then
    source $HOME/.tdcompletion.sh
fi

# Complete the `docker` command if possible.
if [ -f $HOME/.dockercompletion.sh ]; then
    source $HOME/.dockercompletion.sh
fi

# Add the `scripts` dir to the path if possible. See:
# https://github.com/mssola/scripts.
if [ -d $HOME/.scripts ]; then
    export PATH=$HOME/.scripts:$PATH
fi

# Generates a PDF version of the manual of the given command.
function manpdf() {
  man -t "$1" | ps2pdf - "$1.pdf"
}

# Generate the contents for a .gitignore file.
function gi() {
  curl http://www.gitignore.io/api/$@
}

# Finally get RVM straight.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin"
