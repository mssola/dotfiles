#!/usr/bin/env ruby

require 'json'
require 'yaml'

puts JSON.parse(ARGF.read).to_yaml
