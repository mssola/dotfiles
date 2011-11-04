##
# This is the configuration file for Unicorn.


# Feel free to change any of the following constants for your app:
WD = '/my/app/root'
NCORES = `cat /proc/cpuinfo | grep 'cores' | uniq | sed 's/[^0-9]//g'`.to_i

# Setting up the working directory and some paths
working_directory WD
pid "#{WD}/tmp/pids/unicorn.pid"
stderr_path "#{WD}/log/unicorn.my_app_name.sock"
stdout_path "#{WD}/log/unicorn.my_app_name.sock"

# We want one worker per core.
worker_processes NCORES

# Listen on a Unix domain socket. I'm using a shorter backlog
# for quicker failover when busy.
listen '/tmp/unicorn.my_app_name.sock', backlog: 64

# nuke workers after 30 seconds instead of 60 seconds (the default)
timeout 30

# Preloading app for memory saving on COW-friendly GC's
preload_app true
GC.respond_to?(:copy_on_write_friendly=) and
  GC.copy_on_write_friendly = true


before_fork do |server, worker|
  ##
  # The following is highly recomended for Rails + "preload_app true"
  # as there's no need for the master process to hold a connection
  defined?(ActiveRecord::Base) and
    ActiveRecord::Base.connection.disconnect!

  ##
  # When sent a USR2, Unicorn will suffix its pidfile with .oldbin and
  # immediately start loading up a new version of itself (loaded with a new
  # version of our app). When this new Unicorn is completely loaded
  # it will begin spawning workers. The first worker spawned will check to
  # see if an .oldbin pidfile exists. If so, this means we've just booted up
  # a new Unicorn and need to tell the old one that it can now die. To do so
  # we send it a QUIT.
  #
  # Using this method we get 0 downtime deploys.
  old_pid = "#{WD}/tmp/pids/unicorn.pid.oldbin"
  if File.exists?(old_pid) && server.pid != old_pid
    begin
      Process.kill("QUIT", File.read(old_pid).to_i)
    rescue Errno::ENOENT, Errno::ESRCH
      # someone else did our job for us
    end
  end
end

after_fork do |server, worker|
  # The following is *required* for Rails + "preload_app true",
  defined?(ActiveRecord::Base) and
    ActiveRecord::Base.establish_connection
end
