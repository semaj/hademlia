require 'socket'
require 'ecdsa'

NUM_NODES = 10

@sockets = {} # Node ID => UNIXSocket
@messages = {} # Node ID => [messages to send]
@threads = []

node_names = NUM_NODES.times.map do |i|
  name = rand(100000000..999999999).to_s(16)
  puts "#{i} : #{name}"
  name
end

node_names.each do |name|
  @threads << Thread.new { system("stack exec hademlia-exe #{name}") }
end

sleep 3 # wait for them to start their servers

node_names.each do |name|
  @sockets[name] = UNIXSocket.new("/tmp/#{name}.socket")
  @messages[name] = []
end

def parse_msg(msg)
end

loop do
  @sockets.each do |name, socket|
    parse_msg(socket.read.chomp)
  end
end
