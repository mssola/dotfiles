export KF5=$KDEDIR/kf5
export QTDIR=/opt/qt5/qtbase
export XDG_DATA_DIRS=$KF5/share:$XDG_DATA_DIRS:/usr/share
export XDG_CONFIG_DIRS=$KF5/etc/xdg:$XDG_CONFIG_DIRS:/etc/xdg
export PATH=$KF5/bin:$QTDIR/bin:$PATH
export QT_PLUGIN_PATH=$KF5/lib/plugins:$KF5/lib64/plugins:$KF5/lib/x86_64-linux-gnu/plugins:$QTDIR/plugins:$QT_PLUGIN_PATH
export QML2_IMPORT_PATH=$KF5/lib/qml:$KF5/lib64/qml:$KF5/lib/x86_64-linux-gnu/qml:$QTDIR/qml
export QML_IMPORT_PATH=$QML2_IMPORT_PATH
export KDE_SESSION_VERSION=5
export KDE_FULL_SESSION=true
export XDG_DATA_HOME=$HOME/.local5
export XDG_CONFIG_HOME=$HOME/.config5
export XDG_CACHE_HOME=$HOME/.cache5
export CMAKE_PREFIX_PATH=$KF5:$CMAKE_PREFIX_PATH

