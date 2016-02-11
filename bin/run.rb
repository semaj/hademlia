require 'socket'
require 'ecdsa'

NUM_NODES = 4

@messages = {} # Node ID => [messages to send]
@pids = []

node_names = NUM_NODES.times.map do |i|
  name = rand(1024..65535)
  puts "#{i} : #{name}"
  name
end

begin
  node_names.reduce(nil) do |bootstrap, name|
    @pids << spawn("stack exec hademlia-exe #{name} #{bootstrap}")
    sleep 0.5
    name
  end
  loop { }
ensure
  @pids.map { |p| Process.kill(:SIGINT, p) }
  puts "-- PIDS KILLED --"
end
