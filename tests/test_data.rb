#!/bin/env ruby

require 'securerandom'
require 'json'
require 'net/http'

def time_rand from = 0.0, to = Time.now
  Time.at(from + rand * (to.to_f - from.to_f)).strftime("%Y-%m-%dT%H:%M:%S.0")
end
def put_data(data)
  port = 8090
  host = "127.0.0.1"
  path = "/metrics"

  req = Net::HTTP::Put.new(path, {'Content-Type' => 'application/json'})
  req.body = data
  response = Net::HTTP.new(host, port).start {|http| http.request(req) }
  puts response.code
end

nbdata=ARGV[0].to_i || 1000

uuids = {}
10.times do
  uuid = SecureRandom.uuid
  uuids[uuid] = []

  5.times do
    uuids[uuid] << SecureRandom.uuid
  end
end

metrics = {
  "cpu.usage" => 10..100,
  "mem.usage" => 1024..4096,
  "network.incoming.bytes" => 0..1000,
  "network.outgoing.bytes" => 0..1000,
  "volume.size" => 1..1024
}

nbdata.times do
  uuid = uuids.keys.sample
  resource = uuids[uuid].sample
  metric = metrics.keys.sample
  value = metrics[metric].to_a.sample
  date = time_rand Time.local(2014, 1, 1)

  puts "** #{uuid}##{resource} :: #{date} - #{metric} = #{value}"

  data = {
    "project_id" => uuid,
    "resource_id" => resource,
    "metrics" => { metric => value },
    "date" => date
  }

  put_data data.to_json
end
