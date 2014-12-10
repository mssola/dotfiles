
## KTextEditor (KTE)

This is the configuration for KTE users (KWrite, Kate, KDevelop and Kile). It
basically contains:

1. Global indentation configs (`.kateconfig` files).
2. Color schemas.

The `.kateconfig` files will get installed automatically through the
main `install.sh` script. The color schemas have to be imported manually
though, because it's not possible to predict the installation path.

There are two `.kateconfig` files:

1. The `global` one, that is installed in the `$HOME` directory. This one
   defines the global indentation settings that differ from the default
   settings as given in the configuration dialog (for me it's 4 spaces no
   tabs).
2. The `kde` one, that is installed by default in the `$HOME/Projects/kde`
   directory. This config file overrides the global one so the end config
   matches the one required by the KDE style guide.

My color schemas are the ones that you can find inside the `colors` directory.
It contains the following color schemas:

1. `mssola`: my first personalized color schema (oh good ol' times :P). It's a
   light color schema that is pretty similar to the old defaults from KDE. It
   has not been ported to KDE Frameworks 5, and I don't really use it anymore.
2. `mssola-solarized`: a Dark Solarized-like schema. It has not been ported to
   KDE Frameworks 5, even though I have plans to do it eventually.
3. `xoria256`: a port of the
   [xoria256](http://www.vim.org/scripts/script.php?script_id=2140) schema.
   It has not been fully ported, but it's in the works. This is the one that
   I'm mostly using currently.

