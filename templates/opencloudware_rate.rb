#
# Copyright 2015 Sebastien Badia <sbadia@redhat.com>
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
#

require 'ebill'
require 'uri'
require 'net/http'

# Basic billing values
#
# Cpu    : pct * 100
# Memory : 0.02€ for 1Mo used
# Storage: 0.10€ for 1Mo used
# Network: 1.50€ for 1Mb/s used (in/out)

$metrics = {
  "cpu.usage"    => {
    "delta" => 1.0e2,  # 0.67490907%
    "total" => 0,
    "euros" => 100     # percent used x 100 for the euro value
  },
  "memory.total" => {
    "delta" => 1.0e-6, # 1051,701248 Mo
    "total" => 0,
    "euros" => 0.02    # 0.02€ for 1Mo
  },
  "storage.used" => {
    "delta" => 1.0e-4, # 204,5540 Mo
    "total" => 0,
    "euros" => 0.10    # 0.10€ for 1Mo
  },
  "network.0.tx" => {
    "delta" => 1.0e-7, # 59426348 (5.9Mbs)
    "total" => 0,
    "euros" => 1.50    # 1.5€ for 1Mb/s
  },
  "network.0.rx" => {
    "delta" => 1.0e-7, # 21856312 (2.1Mbs)
    "total" => 0,
    "euros" => 1.50    # 1.5€ for 1Mb/s
  }
}

def rate(data)
  EBill.info("eBill version #{EBill::VERSION}")

  json = EBill.to_json(data)
  data = json[:ok]

  # {"project_id"=>"ea08cc13-1c54-4044-bb67-b0529cf2e634", "resource_id"=>"ae80d47e-bf70-43ff-be06-2fe623e0485b", "metadatas"=>[], "metric"=>"network.0.rx", "value"=>"60137855", "date"=>"2015-08-31T10:34:21.0"}

  def get_metric(vmid, key)
    port = 9998
    host = '10.197.180.205'
    path = '/proactive-watch/metric'
    req = Net::HTTP::Get.new(path, {'resource' => vmid, 'key' => key})
    res = Net::HTTP.new(host, port).start {|http| http.request(req)}
    code = res.code.to_i
    if (code >= 200 && code < 300) then
      return res.body
    end
  end

  def get_cost(data)
    month = ((365.25/12) * (60*60*24)).to_i
    cost = 0

    data.each do |r|
      if r['date'].to_i < (Time.now.to_i + month)
        # Metric total = All records * delta * euro multiplier
        if $metrics[r['metric']] == 'cpu.usage'
          #cores = get_metric(r['resource_id'], 'cpu.cores')
          #pct_free = ((get_metric(r['resource_id'], 'storage.total').to_i - $metrics[r['value']].to_i) / 5000)
          cores = 1
          $metrics[r['metric']]['total'] += (((r['value'].to_f * $metrics[r['metric']]['delta'].to_f) * $metrics[r['metric']]['euros'].to_f) * cores.to_i)
        else
          $metrics[r['metric']]['total'] += ((r['value'].to_f * $metrics[r['metric']]['delta'].to_f) * $metrics[r['metric']]['euros'].to_f)
        end
      end
    end

    $metrics.keys.each do |k|
      $metrics[k]['total'] = ($metrics[k]['total']).round(2)
      cost += $metrics[k]['total']
    end

    cost = cost.round(2)
    EBill.info("Cost (#{cost}) for the month: #{$metrics}")
    return "['#{cost}', '#{$metrics}']"
  end

  rc = get_cost(data)
  EBill.ok(rc)
end
