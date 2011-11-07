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

# Rake completion
complete -C $HOME/.rake_completion -o default rake

# Paths
export KDEDIR=/home/mssola/kde
export KDEDIRS=$KDEDIR
export KDEV=$HOME/Projects/kde
export RUBY=$KDEV/kdev-ruby

PATH=$KDEDIR/bin:$PATH

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Including shortucts, idea from https://github.com/fxn/dotfiles
GO_SHORTCUTS=(
  rem
  $HOME/Desktop/fib/ac/pxc/rem

  ruby
  $HOME/Projects/kde/kdev-ruby
)


function go {
  local target=$1
  local len=${#GO_SHORTCUTS[@]}
  for (( i=0; i<$len; i+=2 ));
  do
    if [[ "$1" = "${GO_SHORTCUTS[$i]}" ]]; then
      cd "${GO_SHORTCUTS[$i+1]}"
      return
    fi
  done
  echo "unknown shortcut"
}
