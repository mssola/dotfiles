# frozen_string_literal: true

# Copyright (C) 2014-2022 Miquel Sabaté Solà <mikisabate@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require 'irb/completion'
require 'irb/ext/save-history'
require 'rubygems'
require 'pp'

# Colorize!
# rubocop:disable Lint/SuppressedException
begin
  require 'wirble'

  Wirble.init
  Wirble.colorize
rescue LoadError
end
# rubocop:enable Lint/SuppressedException

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
  `ls`.split("\n")
end

alias p pp
