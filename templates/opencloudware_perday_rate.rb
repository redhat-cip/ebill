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
require 'date'

# Basic billing values
#
# Cpu    : pct * 100
# Memory : 0.02€ for 1Mo used
# Storage: 0.10€ for 1Mo used
# Network: 1.50€ for 1Mb/s used (in/out)

$billing = {}
# billing = {
#   '2015-09-15' => {
#     'cpu'     => 'xxx',
#     'memory'  => 'xxx',
#     'storage' => 'xxx',
#     'network' => 'xxx'
#   }
# }

$metrics = {
  "cpu.usage"    => {
    "delta" => 1.0e2,  # 0.67490907%
    "euros" => 100     # percent used x 100 for the euro value
  },
  "memory.total" => {
    "delta" => 1.0e-6, # 1051,701248 Mo
    "euros" => 0.02    # 0.02€ for 1Mo
  },
  "storage.used" => {
    "delta" => 1.0e-4, # 204,5540 Mo
    "euros" => 0.10    # 0.10€ for 1Mo
  },
  "network.0.tx" => {
    "delta" => 1.0e-7, # 59426348 (5.9Mbs)
    "euros" => 1.50    # 1.5€ for 1Mb/s
  },
  "network.0.rx" => {
    "delta" => 1.0e-7, # 21856312 (2.1Mbs)
    "euros" => 1.50    # 1.5€ for 1Mb/s
  }
}

def rate(data)
  EBill.info("eBill version #{EBill::VERSION}")

  json = EBill.to_json(data)
  data = json[:ok]

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
    data.each do |r|
      day = DateTime.parse(r['date']).strftime('%F')
      e = $billing.fetch(day, { "cpu.usage" => 0, "memory.total" => 0, "storage.used" => 0, "network.0.tx" => 0, "network.0.rx" => 0 })
      # Metric total = All records * delta * euro multiplier
      e[r['metric']] += ((r['value'].to_f * $metrics[r['metric']]['delta'].to_f) * $metrics[r['metric']]['euros'].to_f)
      $billing[day] = e
    end

    cost = cost.round(2)
    EBill.info("Billing info #{$billing}")
    return $billing
  end

  rc = get_cost(data)
  EBill.ok(rc)
end
