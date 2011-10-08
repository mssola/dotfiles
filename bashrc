# beautifying ls command
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# vim stuff
export EDITOR=vim
export VISUAL=vim
alias vi=vim

# git completion
source $HOME/.gitcompletion.sh

# prompt with git completion
PS1='\u:\w$(__git_ps1 "\[\033[0;32m\]@\[\033[1;32m\]%s\[\033[0m\]\]") $ '

# Paths
export KDEV_DIR=/home/mssola/kde/kdevelop
export KDEDIR=$KDEV_DIR
export KDEDIRS=$KDEDIR
export KDEV=/home/mssola/Projects/kdevelop
export RUBY=$KDEV/kdev-ruby

PATH=$KDEV_DIR/bin:$PATH

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
