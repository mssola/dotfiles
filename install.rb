##
# Simple script to install all the files to our home
#

#!/usr/bin/env ruby

# You can change your home directory ;)
home = ARGV[0]
home ||= '$HOME'

`cp bashrc #{home}/.bashrc`
`cp irbrc #{home}/.irbrc`
`cp gemrc #{home}/.gemrc`
`cp gitcompletion #{home}/.gitcompletion.sh`
`cp gitconfig #{home}/.gitconfig`
`cp global.gitignore #{home}/.gitignore`

