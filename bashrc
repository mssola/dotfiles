alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# vim stuff
export EDITOR=vim
export VISUAL=vim
alias vi=vim

# prompt
PS1='\u:\w $ '

# Paths
export KDEV_DIR=/home/mssola/kde/kdevelop
export KDEDIR=$KDEV_DIR
export KDEDIRS=$KDEDIR
export KDEV=/home/mssola/Projects/kdevelop4
export RUBY=$KDEV/kdev-ruby

PATH=$KDEV_DIR/bin:$PATH
