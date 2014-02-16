# Set LC_ALL always to UTF-8
export LC_ALL=en_US.UTF-8

# beautifying ls command
lsc="ls"
if [ `uname` == "Darwin" ]; then
  lsc="gls"
fi
alias ls='$lsc --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='$lsc -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='$lsc -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

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
export KDEDIR=$HOME/kde
export KDEDIRS=$KDEDIR
export KDEV=$HOME/Projects/kde
export KRUBY=$KDEV/kdev-ruby

# Export the standard paths to include KDE
export PATH=$KDEDIR/bin:/usr/local/heroku/bin:$PATH
export LD_LIBRARY_PATH=$KDEDIR/lib:$LD_LIBRARY_PATH
export XDG_DATA_DIRS=$XDG_DATA_DIRS:$KDEDIR/share

# Export the CMake paths so it searches for KDE in the right places
export CMAKE_PREFIX_PATH=$KDEDIR:$CMAKE_PREFIX_PATH
export CMAKE_LIBRARY_PATH=$KDEDIR/lib:$CMAKE_LIBRARY_PATH
export CMAKE_INCLUDE_PATH=$KDEDIR/include:$CMAKE_INCLUDE_PATH

# Function to whip the KDevelop cache
alias wduchain='rm -rf $HOME/.cache/kdevduchain/*'

# Alias bundle exec
alias be='bundle exec'

# Go things
export GOROOT=$HOME/Projects/go
export GOPATH=$HOME/Projects/golang
export PATH=$HOME/Projects/golang/bin:$HOME/Projects/go/bin:$PATH

# If Go exists in the system, bring its bash completion in.
if [ -d "$GOROOT" ]; then
  source "$GOROOT/misc/bash/go"
fi

# Completion for the review command. See: https://github.com/mssola/review
source $HOME/.review_completion.sh

# The g utility. See: https://github.com/mssola/g
source $HOME/.g.sh
source $HOME/.gcompletion.sh

# Finally get RVM straight.
source $HOME/.rvm/scripts/rvm

