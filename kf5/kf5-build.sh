#!/bin/bash


# Initial check.
if [ -z "$KF5" ]; then
  echo "You have to set the \$KF5 environment variable."
  exit 1
fi

# Create the $KF5 directory if it doesn't exist yet.
mkdir -p $KF5

# Getting CMake from git.
if [ ! -d cmake ]; then
  git clone git://cmake.org/cmake.git
  cd cmake
  ./configure --prefix=$KF5
  make -j 10 install
  cd ..
fi

# Extra CMake Modules (ECM)
if [ ! -d extra-cmake-modules ]; then
  git clone git://anongit.kde.org/extra-cmake-modules
  cd extra-cmake-modules
  cmake -DCMAKE_INSTALL_PREFIX=$KF5 .
  make install
  cd ..
fi

# The rest of the packages.
for pkg in $(cat list); do
  # Clone or pull.
  if [ -d $pkg ]; then
    cd $pkg
    git down
    mkdir -p build && cd build
  else
    git clone git://anongit.kde.org/$pkg
    mkdir -p $pkg/build
    cd $pkg/build
  fi

  # Compile and install.
  cmake .. -DCMAKE_INSTALL_PREFIX=$KF5
  make -j 10 install
  cd ../..
done

