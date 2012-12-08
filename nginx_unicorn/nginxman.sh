#!/bin/bash


_cfgdir=/etc/nginx
_tmpdir=/var/lib/nginx
_nginx=~/nginx-tmp
_version="trunk"
_configure="./configure"


# Fetch and extract SPDY
if [ $_version == "trunk" ]; then
  echo -e "\033[32mFetching NGinx using svn (trunk)\033[0m"
  svn co svn://svn.nginx.org/nginx/trunk $_nginx
  _configure="./auto/configure"
else
  echo -e "\033[32mFetching NGinx using wget ($_version)\033[0m"
  wget http://nginx.org/download/nginx-$_version.tar.gz
  tar xzvf nginx-$_version.tar.gz
  rm nginx-$_version.tar.gz
  mv nginx-$_version $_nginx
fi
cd $_nginx

# Fetch and apply the Nginx SPDY patch
echo -e "\033[32mFetching SPDY patch\033[0m"
wget http://nginx.org/patches/spdy/patch.spdy.txt
patch -p0 < patch.spdy.txt

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
  --with-http_stub_status_module \
  
# Compile & install
echo -e "\033[32mCompiling\033[0m"
make -j 4
echo -e "\033[32mInstalling\033[0m"
sudo make install

# Cleanup
echo -e "\033[32mCleaning up\033[0m"
rm -rf $_nginx
