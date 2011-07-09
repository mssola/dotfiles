#!/usr/bin/env ruby
#
# Author: Miquel Sabaté
# http://github.com/mssola/dotfiles


require 'irb/completion'
require 'irb/ext/save-history'
require 'rubygems'
require 'pp'
require 'wirble'


# Colorize!
Wirble.init
Wirble.colorize

# Simple prompt
IRB.conf[:PROMPT_MODE] = :SIMPLE

# History
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = File.expand_path('~/.irb_history')

# Auto-indentation
IRB.conf[:AUTO_INDENT] = true

# Use readline
IRB.conf[:USE_READLINE] = true

# List object methods
def local_methods(obj = self)
  (obj.methods - obj.class.superclass.instance_methods).sort
end

# Reload this .irbrc
def reload!
  load __FILE__
end

# Beautify ls
def ls
  %x{ls}.split("\n")
end

alias p pp
