#!/usr/bin/env bash

set -e

# Enfore BUSYBOX_SRC and QEMU_KERNEL.
if [ -z "$BUSYBOX_SRC" ]; then
    echo "run-qemu (error): you need to define BUSYBOX_SRC."
    exit 1
fi
if [ -z "$QEMU_KERNEL" ]; then
    echo "run-qemu (error): you need to define QEMU_KERNEL with the Image to be used."
    exit 1
fi

# Configurable variables.
QEMU_CPU=${QEMU_CPU:-max}
QEMU_SMP=${QEMU_SMP:-4}
EXTRA=${EXTRA:-}

# Cleanup older environments.
rm -rf "$BUSYBOX_SRC/initramfs/home"

while [[ $# -gt 0 ]]; do
    case $1 in
    -g | --gdb)
        EXTRA="-s -S"
        shift
        ;;
    -h | --help)
        SHOW_HELP=1
        shift
        ;;
    -*)
        echo "run-qemu (error): unknown option '$1'."
        exit 1
        ;;
    *)
        mkdir -p "$BUSYBOX_SRC/initramfs/home/$(whoami)"
        cp "$1" "$BUSYBOX_SRC/initramfs/home/$(whoami)/"
        shift # past argument
        ;;
    esac
done

# Print the show message if needed.
if [ -n "$SHOW_HELP" ]; then
    cat <<HERE
usage: run-riscv-qemu.sh [OPTIONS]

Run QEMU/KVM tailored to my needs for testing the Linux Kernel.

  -g, --gdb             Wait for a GDB connection.
  -h, --help            Show this message.

Environment variables that can be used the further configure the run:

  - EXTRA: extra arguments to pass. Default: (empty).
  - QEMU_CPU: CPU model to use (i.e. argument for '-cpu'). Default: 'max'.
  - QEMU_SMP: number of cores (i.e. argument for '-smp'). Default: 4.
HERE
    exit 0
fi

##
# Generate the initramfs.

pushd "$BUSYBOX_SRC/initramfs"
find . -print0 | cpio --null -o --format=newc | gzip -9 >../initramfs.cpio.gz
popd

##
# Actual run.

set -x

qemu-system-riscv64 -machine virt -nographic \
    -cpu "$QEMU_CPU" \
    -smp "$QEMU_SMP" \
    -kernel "$QEMU_KERNEL" \
    -initrd "$BUSYBOX_SRC/initramfs.cpio.gz" \
    -append "root=/dev/ram" \
    -append nokaslr \
    -append ftrace_dump_on_oops \
    -append sysctl.kernel.panic_on_rcu_stall=1 \
    $EXTRA
