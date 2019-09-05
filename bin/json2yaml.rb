#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'yaml'

puts JSON.parse(ARGF.read).to_yaml
