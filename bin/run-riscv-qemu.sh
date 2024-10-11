#!/usr/bin/env sh

set -e

CORES=${CORES:-4}
EXTRA=${EXTRA:-}
REPO=${REPO:-riscv}
KERNEL=${KERNEL:-~/src/git.kernel.org/linux/kernel/$REPO/arch/riscv/boot/Image}
INITRD=${INITRD:-~/src/busybox/1.36.1/initramfs.cpio.gz}

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
        echo "run-qemu (warning): argument '$1' will be ignored."
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

  - CORES: number of cores (i.e. argument for '-smp'). Default: 4.
  - EXTRA: extra arguments to pass. Default: (empty).
  - INITRD: the initial ramdisk for the run (e.g. argument for '-initrd'). Default: "$INITRD".
  - KERNEL: the kernel to be passed (i.e. argument for '-kernel'). Default: "$KERNEL".
  - REPO: the Linux repository to be used. Default: "riscv".
HERE
    exit 0
fi

qemu-system-riscv64 -machine virt -nographic \
    -smp "$CORES" \
    -kernel "$KERNEL" \
    -initrd "$INITRD" \
    -append "root=/dev/ram" \
    -append nokaslr \
    -append ftrace_dump_on_oops \
    -append sysctl.kernel.panic_on_rcu_stall=1 \
    $EXTRA
