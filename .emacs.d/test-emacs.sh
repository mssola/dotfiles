#!/bin/bash -ex
# Some parts kindly taken from: https://github.com/purcell/emacs.d

if [ -n "$TRAVIS" ]; then
    # We still need to have an .emacs.d directory, so emacs-async (dependency of
    # some other package) is happy.
    export HOME=$PWD/..
    ln -s emacs ../.emacs.d
fi

##
# Linters.

mkdir -p vendor
wget -q -O vendor/elisp-lint.el https://raw.githubusercontent.com/gonewest818/elisp-lint/master/elisp-lint.el
wget -q -O vendor/package-lint.el https://raw.githubusercontent.com/purcell/package-lint/master/package-lint.el
LOAD_PATH="-L $(readlink -f -- vendor)"

# shellcheck disable=SC2039
pushd .
for i in . lisp; do
    cd "$i"
    # shellcheck disable=SC2010
    # shellcheck disable=SC2035
    files="$(ls *.el | grep -v 'custom.el' | grep -v 'abbrevs.el' | grep -v 'lisp-autoloads.el' | grep -v '.emacs.d-autoloads.el' | grep -v 'soria-theme.el' | xargs)"

    # shellcheck disable=SC2086
    ${EMACS:=emacs} -Q --batch ${LOAD_PATH} -l elisp-lint.el -f elisp-lint-files-batch $files
    # shellcheck disable=SC2086
    ${EMACS:=emacs} -Q --batch ${LOAD_PATH} -l package-lint.el -f package-lint-batch-and-exit $files
done

# shellcheck disable=SC2039
popd
rm -r vendor

##
# Startup.

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
