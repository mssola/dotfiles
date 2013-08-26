#!/bin/bash


# Change this values to your own needs.
_cfgdir=/etc/nginx
_tmpdir=/var/lib/nginx
_nginx=/tmp/nginx
_version="default"
_configure="./configure"
_pwd=`pwd`


# Fetch and extract NGinx
if [ $_version == "default" ]; then
  echo -e "\033[32mFetching NGinx using hg (default)\033[0m"
  hg clone http://hg.nginx.org/nginx/ $_nginx
  _configure="./auto/configure"
else
  echo -e "\033[32mFetching NGinx using wget ($_version)\033[0m"
  wget http://nginx.org/download/nginx-$_version.tar.gz
  tar xzvf nginx-$_version.tar.gz
  rm nginx-$_version.tar.gz
  mv nginx-$_version $_nginx
fi
cd $_nginx

# Configure
echo -e "\033[32mConfiguring\033[0m"
$_configure \
  --prefix=$_cfgdir \
  --conf-path=$_cfgdir/nginx.conf \
  --sbin-path=/usr/sbin/nginx \
  --pid-path=/var/run/nginx.pid \
  --lock-path=/var/lock/nginx.lock \
  --user=http --group=http \
  --http-log-path=/var/log/nginx/access.log \
  --error-log-path=/var/log/nginx/error.log \
  --http-client-body-temp-path=$_tmpdir/client-body \
  --http-proxy-temp-path=$_tmpdir/proxy \
  --http-fastcgi-temp-path=$_tmpdir/fastcgi \
  --http-scgi-temp-path=$_tmpdir/scgi \
  --http-uwsgi-temp-path=$_tmpdir/uwsgi \
  --with-imap --with-imap_ssl_module \
  --with-ipv6 --with-pcre-jit \
  --with-file-aio \
  --with-http_dav_module \
  --with-http_geoip_module \
  --with-http_gzip_static_module \
  --with-http_realip_module \
  --with-http_ssl_module \
  --with-http_spdy_module \
  --with-http_stub_status_module \
  --with-cc-opt="-O3"
  
# Compile & install
echo -e "\033[32mCompiling\033[0m"
make -j 4
echo -e "\033[32mInstalling\033[0m"
sudo make install
sudo mkdir -p /var/lib/nginx

# Cleanup
echo -e "\033[32mCleaning up\033[0m"
rm -rf $_nginx
