##
# Simple script to install all the files to our home
#

#!/usr/bin/env ruby

# Check for the wirble gem since the .irbrc file requires
# this gem to successfully work.
begin
  require 'wirble'
rescue LoadError
  puts 'The .irbrc file requires wirble and you haven\'t this gem installed!'
end

# You can change your home directory ;)
home = ARGV[0]
home ||= '$HOME'

`cp bashrc #{home}/.bashrc`
`cp irbrc #{home}/.irbrc`
`cp gemrc #{home}/.gemrc`
`cp gitcompletion #{home}/.gitcompletion.sh`
`cp gitconfig #{home}/.gitconfig`
`cp global.gitignore #{home}/.gitignore`
`cp vimrc #{home}/.vimrc`
`cp hgrc #{home}/.hgrc`
`cp rake_completion #{home}/.rake_completion`
`chmod +x #{home}/.rake_completion`
