require 'socket'
require 'ecdsa'

NUM_NODES = 10

@messages = {} # Node ID => [messages to send]
@threads = []

node_names = NUM_NODES.times.map do |i|
  name = rand(1024..65535)
  puts "#{i} : #{name}"
  name
end


node_names.reduce(nil) do |bootstrap, name|
  @threads << Thread.new do
    system("stack exec hademlia-exe #{name} #{bootstrap}")
  end
  sleep 1
  name
end

@threads.map(&:join)
