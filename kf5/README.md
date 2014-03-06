# KDE Frameworks

This directory has all the files that I use to compile KDE Frameworks. The
following packages are not included: kdewebkit and kde4support.
Moreover, the KTextEditor library is not included in the list of modules
to be compiled. This is because it's the only library from Frameworks
that I contribute and, therefore, I want to keep it separately. I will
remove this directory when the KDE Frameworks is stable and all the
KDE apps are ported to Frameworks.

This directory assumes that you have my Git configuration (things like the
"down" alias, the "kde:" prefix, etc.). This directory also assumes that
you have installed all the dependencies as specified in:

    https://community.kde.org/Frameworks/Building

In order to compile everything, perform the following commands in the
directory where all the KDE Frameworks sources should go:

    $ source kf5.sh
    $ ./kf5-build.sh

This configuration assumes that:

  * The KF5 libraries will be installed at $HOME/kf5.
  * You have Qt 5 installed at /opt/qt5.

Happy hacking!
