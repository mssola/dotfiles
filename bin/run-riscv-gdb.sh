#!/usr/bin/env sh

set -e

TUI=""
BASE=${BASE:-~/src/git.kernel.org/linux/kernel}
PROFILE=${PROFILE:-riscv}
REPO=${REPO:-riscv}
VMLINUX=${VMLINUX:-$BASE/$REPO/vmlinux}

while [[ $# -gt 0 ]]; do
  case $1 in
  -t | --tui)
    TUI="-tui"
    shift
    ;;
  -h | --help)
    SHOW_HELP=1
    shift
    ;;
  -*)
    echo "run-gdb (error): unknown option '$1'."
    exit 1
    ;;
  *)
    echo "run-gdb (warning): argument '$1' will be ignored."
    shift # past argument
    ;;
  esac
done

# Print the show message if needed.
if [ -n "$SHOW_HELP" ]; then
  cat <<HERE
usage: run-gdb.sh [OPTIONS]

Start a connection to a running QEMU/KVM machine.

  -h, --help            Show this message.
  -t, --tui             Enable TUI mode.

Environment variables that can be used the further configure GDB:

  - BASE: the base kernel location. That is, the directory where kernel trees live. Default: "$BASE".
  - PROFILE: the GDB profile from the default base kernel location. Default: "riscv".
  - REPO: the Linux repository to be used. Default: "riscv".
  - VMLINUX: path to 'vmlinux' so to load symbols. Default: "$VMLINUX".
HERE
  exit 0
fi

gdb $TUI \
  --command $BASE/$PROFILE.gdb \
  $VMLINUX
