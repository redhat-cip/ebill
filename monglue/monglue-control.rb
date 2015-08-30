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


# curl -X GET -H 'resource: ae80d47e-bf70-43ff-be06-2fe623e0485b' -H 'key: memory.actualfree' http://10.197.180.205:9998/proactive-watch/metric
#
require 'uri'
require 'redis'
require 'json'
require 'net/http'

# 1/ on récupère la liste des id a monitorer
redis = Redis.new
db = 'ocwbill'
vmsid = redis.lrange(db,'0','-1')
monit = ARGV[0] || '10.197.180.205'
ebill = ARGV[1] || '10.197.180.216'
#vmsid = ['ae80d47e-bf70-43ff-be06-2fe623e0485b']

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

def put_data(vmid, data, host)
  port = 8090
  path = '/metrics'

  req = Net::HTTP::Put.new(path, {'Content-Type' => 'application/json'})
  req.body = data
  response = Net::HTTP.new(host, port).start {|http| http.request(req) }
  puts "#{Time.now} :: #{vmid} :: #{response.code}"
end

# le service de monitoring ne supporte actuellement pas la demande de plusieurs metriques
# dans un meme GET, le retour est un plain-text avec la valeur
vmsid.each do |vmid|
  project_id = 'ea08cc13-1c54-4044-bb67-b0529cf2e634'
  # 2/ on demande les metriques en question au service de billing
  data = {
    'project_id' => project_id,
    'resource_id' => vmid,
    'metrics' => {
      'cpu.usage' => get_metric(vmid, 'cpu.usage', monit),
      'memory.total' => get_metric(vmid, 'memory.total', monit),
      'storage.used' => get_metric(vmid, 'storage.used', monit),
      'network.0.tx' => get_metric(vmid, 'network.0.tx', monit),
      'network.0.rx' => get_metric(vmid, 'network.0.rx', monit)
    }
  }
  # 3/ on envoie ces infos a ebill pour historiser
  put_data(vmid,data.to_json, ebill)
end
