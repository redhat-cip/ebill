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

require 'redis'
require 'sinatra'
require 'json'

redis = Redis.new
db = 'ocwbill'

set :port, 8070
set :bind, '0.0.0.0'
set :method_override, true

get '/' do
  "monglue: please read the documentation\n"
end

post "/" do
  request.body.rewind
  data = JSON.parse request.body.read
  redis.rpush(db, data['vmid'])
  "Start billing for vm #{data['vmid']}\n"
end

get '/bill/:vmid' do
  redis.rpush(db, params['vmid'])
  "Start billing for vm #{params['vmid']}\n"
end

delete '/bill/:vmid' do |v|
  redis.lrem(db,'0',v)
  "Stop billing for vm #{v}\n"
end

get '/list' do
  redis.lrange(db,'0','-1').join(',')
end
