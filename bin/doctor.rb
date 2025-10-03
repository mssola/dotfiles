# frozen_string_literal: true

# Copyright (C) 2025-Ω Miquel Sabaté Solà <mikisabate@gmail.com>
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

require 'fileutils'
require 'pathname'

##
# Early checks.

raise 'Only Linux on x86/ARM64 supported!' if RUBY_PLATFORM != 'x86_64-linux' &&
                                              RUBY_PLATFORM != 'aarch64-linux'
raise 'Only CRuby, sorry!' if RUBY_ENGINE != 'ruby'

##
# Util functions.

def has?(executable:)
  exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']

  ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
    exts.each do |ext|
      exe = File.join(path, "#{executable}#{ext}")
      return exe if File.executable?(exe) && !File.directory?(exe)
    end
  end

  false
end

##
# Mandatory binaries.

puts '0. Checking on binaries needed for the system.'

pending = []
%w[git curl emacs vim mise go cargo ruby rg mbsync parallel notify-send].each do |bin|
  pending << bin unless has?(executable: bin)
end

if pending.size.positive?
  puts "You need to install: #{pending.join(', ')}."
  exit 0
end

##
# Symbolic links and all that jazz.

puts '1. Creating symlinks.'

raise 'Dude, something is really funky on your HOME!' unless Dir.exist?(Dir.home)

SOURCE = Pathname.new("#{File.dirname(__FILE__)}/..").realpath

# Create the systemd local configuration if it doesn't exist already.
systemd_user_path = File.join(Dir.home, '.config/systemd/user/')
FileUtils.mkdir_p(systemd_user_path) unless File.exist?(systemd_user_path)

files = %w[
  bin .vim
  .gnupg/gpg-agent.conf .gnupg/gpg.conf
  .config/alacritty .config/emacs  .config/gdb  .config/ghostty  .config/irb
  .config/mise  .config/msmtp  .config/mimeapps.list  .config/user-dirs.dirs
  .config/systemd/user/do-backup.service .config/systemd/user/do-backup.timer
  .bash_profile .bashrc .clang-format .gemrc .gitconfig .gitconfig-suse
  .hunspell_ca_ES .inputrc .mbsyncrc .psqlrc .rvmrc .tmux.conf .valgrindrc .vimrc
]

files.each do |f|
  dst = File.join(Dir.home, f)
  FileUtils.rm_f(dst)

  src = File.join(SOURCE, f)
  puts dst if ENV['VERBOSE'].to_s == '1'
  File.symlink(src, dst)
end

# The global .gitignore needs a rename.
dst = File.join(Dir.home, '.gitignore')
puts dst if ENV['VERBOSE'].to_s == '1'
FileUtils.rm_f(dst)
File.symlink(File.join(SOURCE, '.global.gitignore'), dst)

##
# Stuff I'm wildly downloading from the internet.

puts "2. Downloading stuff from the internet."

src_home = File.join(Dir.home, 'src/github.com/mssola')

# The 'g' utility.
if File.exist?(File.join(src_home, 'g'))
  Dir.chdir(File.join(src_home, 'g'))
  puts ">> pulling github.com/mssola/g..."
  system('git pull --rebase')
else
  Dir.chdir(src_home)
  puts ">> cloning github.com/mssola/g..."
  system('git clone git@github.com:mssola/g.git')
end

# And link the relevant files from 'g'.
%w[g.sh gcompletion.sh].each do |file|
  dst = File.join(Dir.home, ".#{file}")
  puts dst if ENV['VERBOSE'].to_s == '1'
  FileUtils.rm_f(dst) if File.exist?(dst)
  File.symlink(File.join(src_home, "g/#{file}"), dst)
end

# The 'soria' theme for GNU Emacs.
if File.exist?(File.join(src_home, 'soria'))
  Dir.chdir(File.join(src_home, 'soria'))
  puts ">> pulling github.com/mssola/soria..."
  system('git pull --rebase')
else
  Dir.chdir(src_home)
  puts ">> cloning github.com/mssola/soria..."
  system('git clone git@github.com:mssola/soria.git')
end

# git-prompt
puts ">> downloading git-prompt.sh from git/contrib..."
dst = File.join(Dir.home, '.git-prompt.sh')
puts dst if ENV['VERBOSE'].to_s == '1'
FileUtils.rm_f(dst) if File.exist?(dst)
system("curl -o #{dst} https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh")
puts ''

##
# Dependencies from programming languages.

puts "3. Installing/updating dependencies from programming languages."

system('go install golang.org/x/tools/gopls@latest')
system('go install golang.org/x/tools/cmd/goimports@latest')

system('gem install rubocop')
system('gem install solargraph')

##
# Optional stuff.

extra = []
unless has?(executable: 'delta')
  extra << 'git-delta'
  puts "[!!] 'git-delta' is not installed: 'magit-delta' won't work!"
end

unless has?(executable: 'mu')
  extra << 'maildir-utils'
  puts "[!!] 'mu/mu4e' is not installed: email won't work!"
end

unless has?(executable: 'mbsync')
  extra << 'isync'
  puts "[!!] 'mbsync' is not installed: fetching emails won't work!"
end

unless has?(executable: 'difft')
  extra << 'difftastic'
  puts "[!!] 'difftastic' is not installed: git won't look as awesome!"
end

git_send_email = "/usr/libexec/git/git-send-email"
if !File.exist?(git_send_email) || !File.executable?(git_send_email)
  extra << 'git-email'
  puts "[!!] 'git send-email' is not installed: you won't be able to send patches via email!"
end

if extra.size.positive?
  print "\n=> You can install all of that with:\n"
  puts "    => zypper in #{extra.join(' ')}"
end

##
# User systemd units.

system('systemctl --user enable do-backup.timer')

##
# Binaries which need to go global.

extra = []
(Dir.glob(SOURCE.join('bin/zypper-*')) << SOURCE.join('bin/e')).map do |src|
  dst = "/usr/bin/#{File.basename(src)}"
  extra << "ln -s #{src} #{dst}" unless File.exist?(dst)
end

if extra.size.positive?
  print "\n=> Extra work: run the following while being *root*.\n\n"
  puts extra.join("\n")
end

puts "Done!"
