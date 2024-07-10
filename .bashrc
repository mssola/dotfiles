##
# Locale.

# Set LC_ALL always to UTF-8.
if [ "$(locale -a | grep ca_ES.utf8)" != "" ]; then
    export LC_ALL=ca_ES.utf8
else
    if [ "$(locale -a | grep en_US.utf8)" != "" ]; then
        export LC_ALL=en_US.utf8
    else
        echo "[Warning] Could not set up a proper locale." \
             "There is probably something missing on your host machine..."
    fi
fi

##
# Utility functions.

# Sources the given file if it really exists.
source_maybe() {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

##
# If we are inside of a VM, chances are that the TTY size is borked. Let's
# resize the TTY to the actual terminal size.

if grep -Fq "hypervisor" /proc/cpuinfo; then
    if [ -x "$(command -v resize)" ]; then
        eval `resize`
    else
      echo "Consider installing the `resize` binary if you want a proper TTY size."
    fi
fi

##
# XDG: sourcing the variables on $HOME/.config/user-dirs.dirs might ease up the
# configuration of other tools.

source_maybe "$HOME/.config/user-dirs.dirs"

##
# Basics.

# Beautifying ls command.
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias lisa='ls -lisa --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# Alias related to cd
alias ..='cd ..'
alias cd..='cd ..'

##
# If `bat` is available, use it as a drop-in replacement for `cat (1)`.

if command -v bat &> /dev/null; then
  alias cat='bat'

  # Use it on man pages too.
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
  export MANROFFOPT="-c"
fi

# Set up fzf key bindings and fuzzy completion
if command -v fzf &> /dev/null; then
  eval "$(fzf --bash)"
fi

##
# Editors & terminals.

# Set GNU Emacs as the default editor. You can find this `emacsclient-a-nw`
# executable in the `bin` directory of my dotfiles. That being said, in the
# `bin` directory there is a globally installed script called `e`. This is the
# script that will be called most of the times from the CLI for reaching out to
# the default editor.
export EDITOR=emacsclient-a-nw
export VISUAL=emacsclient-a-nw
alias vi=vim

# We want a full-fledged 256-color terminal.
TERM=xterm-256color

# Setup the prompt command for terminals that don't touch it by themselves
# (e.g. alacritty).
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

# Set the LESS and the PAGER environment variables.
export LESS="FSRX"
export PAGER=less

# git thingies.
alias gti=git

# Setting up PS1.
PS1='\u:\w$(__git_ps1 "\[\033[0;32m\]@%s\[\033[0m\]\]") $ '

# Introduce `cclip` as a fast way to use `xclip` but selecting the clipboard.
alias cclip='xclip -selection clipboard'

##
# Programming languages and environments.

# Rake completion
if [ -f "$HOME/.rake_completion" ]; then
    complete -C "$HOME/.rake_completion" -o default rake
fi

# Alias bundle exec
alias be='bundle exec'

# After experimenting with GCC & Clang, I'm sticking with GCC.
export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

# Go things
export GOPATH=$HOME

##
# Misc.

# The g utility. See: https://github.com/mssola/g
source_maybe "$HOME/.g.sh"
source_maybe "$HOME/.gcompletion.sh"

# Git prompt.
source_maybe "$HOME/.git-prompt.sh"

# Complete the `docker` command if possible.
source_maybe "$HOME/.dockercompletion.sh"

# Add the `bin` dir to the path if possible. See:
# https://github.com/mssola/dotfiles.
if [ -d "$HOME/bin" ]; then
    export PATH=$HOME/bin:$PATH
fi

# Some packages install inside of ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Some other misc. alias.
alias iosc="osc -A https://api.suse.de"
alias ag='ag --nocolor --path-to-ignore ~/.agignore'
alias random_string="cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1"

# Cargo environment.
source_maybe "$HOME/.cargo/env"

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

# Add the binaries of Doom Emacs into my PATH.
export PATH=$PATH:$HOME/.config/emacs/bin

# If we have Rust's source code, export the special `RUST_SRC_PATH`, since this
# is useful for completion tools.
if command -v rustc &> /dev/null; then
  if [ -d "$(rustc --print sysroot)/lib/rustlib/src/rust/library" ]; then
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
  fi
fi

##
# $ mise activate bash >> .bashrc

if command -v mise &> /dev/null; then
  export MISE_SHELL=bash
  export __MISE_ORIG_PATH="$PATH"

  mise() {
    local command
    command="${1:-}"
    if [ "$#" = 0 ]; then
      command mise
      return
    fi
    shift

    case "$command" in
      deactivate|s|shell)
        # if argv doesn't contains -h,--help
        if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
          eval "$(command mise "$command" "$@")"
          return $?
        fi
        ;;
    esac
    command mise "$command" "$@"
  }

  _mise_hook() {
    local previous_exit_status=$?;
    eval "$(mise hook-env -s bash)";
    return $previous_exit_status;
  };
  if [[ ";${PROMPT_COMMAND:-};" != *";_mise_hook;"* ]]; then
    PROMPT_COMMAND="_mise_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
  fi
  if [ -z "${_mise_cmd_not_found:-}" ]; then
    _mise_cmd_not_found=1
    if [ -n "$(declare -f command_not_found_handle)" ]; then
      _mise_cmd_not_found_handle=$(declare -f command_not_found_handle)
      eval "${_mise_cmd_not_found_handle/command_not_found_handle/_command_not_found_handle}"
    fi

    command_not_found_handle() {
      if mise hook-not-found -s bash -- "$1"; then
        _mise_hook
        "$@"
      elif [ -n "$(declare -f _command_not_found_handle)" ]; then
        _command_not_found_handle "$@"
      else
        echo "bash: command not found: $1" >&2
        return 127
      fi
    }
  fi
fi
