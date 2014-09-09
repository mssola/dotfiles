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
export KF5=$KDEDIR/kf5
export QTDIR=/opt/qt5/qtbase
export XDG_DATA_DIRS=$KF5/share:$XDG_DATA_DIRS:/usr/share
export XDG_CONFIG_DIRS=$KF5/etc/xdg:$XDG_CONFIG_DIRS:/etc/xdg
export PATH=$KF5/bin:$QTDIR/bin:/usr/local/heroku/bin:$PATH
export QT_PLUGIN_PATH=$KF5/lib/plugins:$KF5/lib64/plugins:$KF5/lib/x86_64-linux-gnu/plugins:$QTDIR/plugins:$QT_PLUGIN_PATH
export QML2_IMPORT_PATH=$KF5/lib/qml:$KF5/lib64/qml:$KF5/lib/x86_64-linux-gnu/qml:$QTDIR/qml
export QML_IMPORT_PATH=$QML2_IMPORT_PATH
export KDE_SESSION_VERSION=5
export KDE_FULL_SESSION=true
export CMAKE_PREFIX_PATH=$KF5:$CMAKE_PREFIX_PATH

# Function to whip the KDevelop cache
alias wduchain='rm -rf $HOME/.cache/kdevduchain/*'

# Alias bundle exec
alias be='bundle exec'

# Go things
export GOROOT=$HOME/Projects/go
export GOPATH=$HOME/Projects/golang
export PATH=$HOME/Projects/golang/bin:$HOME/Projects/go/bin:$PATH

# The Review utility. See: https://github.com/mssola/review
export PATH=/opt/review:$PATH
source $HOME/.review_completion.sh

# The g utility. See: https://github.com/mssola/g
source $HOME/.g.sh
source $HOME/.gcompletion.sh

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

