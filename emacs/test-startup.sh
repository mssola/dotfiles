#!/bin/sh -ex
# Kindly taken from: https://github.com/purcell/emacs.d

if [ -n "$TRAVIS" ]; then
    # We still need to have an .emacs.d directory, so emacs-async (dependency of
    # some other package) is happy.
    export HOME=$PWD/..
    ln -s emacs ../.emacs.d
fi

echo "Attempting startup..."

${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (package-user-dir (expand-file-name (concat "elpa-" emacs-version)))
                              (custom-theme-directory default-directory)
                              (user-init-file (expand-file-name "init.el")))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'
echo "Startup successful"
