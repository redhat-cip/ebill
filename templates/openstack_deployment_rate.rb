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

$metrics = {
  "cpu.usage"    => {
    "delta" => 2.0e3,  # 0.0067490907 (0.6%)
    "total" => 0
  },
  "memory.total" => {
    "delta" => 2.0e-8, # 1051701248    (1Go)
    "total" => 0
  },
  "storage.used" => {
    "delta" => 1.0e-4, # 2045540     (204Mo)
    "total" => 0
  },
  "network.0.tx" => {
    "delta" => 1.0e-7, # 59426348   (5.9Mbs)
    "total" => 0
  },
  "network.0.rx" => {
    "delta" => 1.0e-7, # 21856312   (2.1Mbs)
    "total" => 0
  }
}

def rate(data)
  EBill.info("eBill version #{EBill::VERSION}")

  json = EBill.to_json(data)
  data = json[:ok]

  def get_cost(data)
    month = ((365.25/12) * (60*60*24)).to_i
    cost = 0

    data.each do |r|
      if r['date'].to_i < (Time.now.to_i + month)
        $metrics[r['metric']]['total'] += (r['value'].to_f * $metrics[r['metric']]['delta'].to_f)
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
