#!/usr/bin/env ruby
#
# Copyright 2015  Sebastien Badia <sbadia@redhat.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'uri'
require 'redis'
require 'net/http'

redis = Redis.new
db = 'ocwbill'
vmsid = redis.lrange(db,'0','-1')
monit = ARGV[0] || '10.197.180.205'

def get_metric(vmid, key, host)
  port = 9998
  path = '/proactive-watch/metric'

  req = Net::HTTP::Get.new(path, {'resource' => vmid, 'key' => key})
  res = Net::HTTP.new(host, port).start {|http| http.request(req)}
  code = res.code.to_i
  if (code >= 200 && code < 300) then
    return res.body
  end
end

vmsid.each do |vmid|
  res = get_metric(vmid, 'cpu.usage', monit)
  if res.match("Error")
    puts "#{Time.now} :: ko :: resource_id #{vmid} :: remove"
    redis.lrem(db,'0',vmid)
  else
    puts "#{Time.now} :: ok :: resource_id #{vmid} :: keep"
  end
end
