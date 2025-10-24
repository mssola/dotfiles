#!/usr/bin/env bash

set -ex

EXTRA=""
KERNEL=""
DISK=""
while [[ $# -gt 0 ]]; do
    case $1 in
    -d | --disk)
        if [ -z "$2" ]; then
            echo "run-riscv-qemu (error): you have to provide a value for -d/--disk"
            exit 1
        fi

        DISK="$2"
        shift 2
        ;;
    -g | --gdb)
        EXTRA="-s -S"
        shift
        ;;
    -h | --help)
        SHOW_HELP=1
        shift
        ;;
    -k | --kernel)
        if [ -z "$2" ]; then
            echo "run-riscv-qemu (error): you have to provide a value for -k/--kernel"
            exit 1
        fi

        KERNEL="$2"
        shift 2
        ;;
    -*)
        echo "run-riscv-qemu (error): unknown option '$1'."
        exit 1
        ;;
    *)
        if [ ! -z "$BUSYBOX_SRC" ]; then
            mkdir -p "$BUSYBOX_SRC/initramfs/home/$(whoami)"
            cp "$1" "$BUSYBOX_SRC/initramfs/home/$(whoami)/"
        fi

        shift
        ;;
    esac
done

# Print the show message if needed.
if [ -n "$SHOW_HELP" ]; then
    cat <<HERE
usage: run-riscv-qemu.sh [OPTIONS] [files]

Run QEMU/KVM tailored to my needs for testing the Linux Kernel.
  -d, --disk          Path to the disk to be passed into QEMU.
  -g, --gdb           Wait for a GDB connection.
  -h, --help          Show this message.
  -k, --kernel        Path to the Linux kernel image to be used.

After the options you can pass paths to files you want to have inside of your
/home directory (unless '-d/--disk' is defined).
HERE
    exit 0
fi

# If the $KERNEL argument has not been set, then guess one based on the current
# working directory.
if [ -z "$KERNEL" ]; then
    if [ ! -z "$ARCH" ]; then
        if [ -f "arch/$ARCH/boot/Image" ]; then
            KERNEL=$(realpath "arch/$ARCH/boot/Image")
        else
            echo "run-riscv-qemu (error): you need to define '-k/--kernel'."
            exit 1
        fi
    else
        echo "run-riscv-qemu (error): you need to define '-k/--kernel'."
        exit 1
    fi
fi
echo "run-riscv-qemu (info): using '\$KERNEL' as the Linux kernel Image."

##
# If the user provided a disk option, run QEMU with that disk in mind.

if [ ! -z "$DISK" ]; then
    qemu-system-riscv64 -machine virt -nographic \
                        -cpu max -smp 4 -m 8G \
                        -drive file="$DISK",format=qcow2,id=hd0 \
                        -device virtio-net-device,netdev=usernet \
                        -netdev user,id=usernet,hostfwd=tcp::22222-:22 \
                        -kernel "$KERNEL" \
                        -append nokaslr \
                        -append ftrace_dump_on_oops \
                        -append sysctl.kernel.panic_on_rcu_stall=1 \
                        $EXTRA

    exit 0
fi

##
# No disk provided, let's generate an initramfs of our own run run in /dev/ram
# with a tmpfs.

# Enfore BUSYBOX_SRC.
if [ -z "$BUSYBOX_SRC" ]; then
    echo "run-riscv-qemu (error): you need to define BUSYBOX_SRC."
    exit 1
fi

# Cleanup older environments.
rm -rf "$BUSYBOX_SRC/initramfs/home"
mkdir -p "$BUSYBOX_SRC/initramfs/home"

pushd "$BUSYBOX_SRC/initramfs"
find . -print0 | cpio --null -o --format=newc | gzip -9 >../initramfs.cpio.gz
popd

##
# Actual run.

set -x

qemu-system-riscv64 -machine virt -nographic \
    -cpu max -m 8G -smp 4 \
    -device virtio-net-device,netdev=usernet \
    -netdev user,id=usernet,hostfwd=tcp::22222-:22 \
    -kernel "$KERNEL" \
    -initrd "$BUSYBOX_SRC/initramfs.cpio.gz" \
    -append "root=/dev/ram" \
    -append nokaslr \
    -append ftrace_dump_on_oops \
    -append sysctl.kernel.panic_on_rcu_stall=1 \
    $EXTRA
