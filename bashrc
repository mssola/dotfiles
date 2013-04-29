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

# git completion
source $HOME/.gitcompletion.sh
PS1='\u:\w$(__git_ps1 "\[\033[0;32m\]@\[\033[1;32m\]%s\[\033[0m\]\]") $ '

# Rake completion
complete -C $HOME/.rake_completion -o default rake

# KDE Paths
export KDEDIR=/home/mssola/kde
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

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Alias bundle exec
alias be='bundle exec'

# Go things
export GOROOT=$HOME/Projects/go
export GOPATH=$HOME/.go
export PATH=$HOME/.go/bin:$HOME/Projects/go/bin:$PATH

# Including shortcuts, idea from https://github.com/fxn/dotfiles
GO_SHORTCUTS=(
  ruby
  $KRUBY

  build
  $KDEV/build/kdev-ruby
)

function g {
  local target=$1
  local len=${#GO_SHORTCUTS[@]}
  for (( i=0; i<$len; i+=2 )); do
    if [[ "$1" = "${GO_SHORTCUTS[$i]}" ]]; then
      cd "${GO_SHORTCUTS[$i+1]}"
      return
    fi
  done
  echo "unknown shortcut"
}

