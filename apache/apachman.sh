#!/bin/bash


# Check depot_tools from the Chromium guys.
if command -v gclient &>/dev/null; then
  echo -e '\033[32mGood boy, You have depot_tools installed :)\033[0m'
else
  if [ ! -e "/tmp/depot_tools" ]; then
    echo -e '\033[31mOops, depot_tools is not installed, fetching it...\033[0m'
    git clone https://git.chromium.org/chromium/tools/depot_tools.git /tmp/depot_tools
  fi
  export PATH=$PATH:/tmp/depot_tools
fi

# Fetching and compiling a patched mod_ssl
echo -e '\033[32mFetching and compiling a patched mod_ssl from googlers\033[0m'
rm -rf /tmp/mod_spdy
mkdir -p /tmp/mod_spdy && cd /tmp/mod_spdy
mkdir -p modules
gclient config "http://mod-spdy.googlecode.com/svn/tags/current/src"
gclient sync --force
cd src && ./build_modssl_with_npn.sh
cp mod_ssl.so ../modules/mod_ssl.so

# Now it's the turn for mod_spdy
echo -e '\033[32mCompiling mod_spdy\033[0m'
make -j 4 BUILDTYPE=Release
cp out/Release/libmod_spdy.so ../modules/mod_spdy.so

# Words of warning
echo -e "
\033[33mWords of Warning\033[0m

  The whole process has compiled two Apache modules into /tmp/mod_spdy/modules/
  You're responsible for installing these two modules where necessary (each
  Linux distribution has its own path policies). This script assumes that you
  previously had a well-configured mod_ssl, so no more configuration is needed
  here. If that is the case, then you also have to setup mod_spdy. To do so,
  add: \"LoadModule spdy_module /some/path/mod_spdy.so\" into the Apache
  configuration and create a configuration file for SPDY like this:

    $ echo \"SpdyEnabled on\" | sudo tee /path/to/conf/spdy.conf

  After this, you just have to restart the Apache daemon and you're fine :)"
