require 'ebill'

# Cout par heure par instance
VM_COST = {
  "m1.micro"   => {
  "unix"    => {:initial => 51, :usage => 0.003, :unit => "hour"},
  "rhel"    => {:initial => 51, :usage => 0.063, :unit => "hour"},
  "windows" => {:initial => 63, :usage => 0.005, :unit => "hour"},
  "*"       => {:initial => 55, :usage => 0.024, :unit => "hour"}
},
  "m1.small"  => {
  "unix"    => {:initial => 102, :usage => 0.006, :unit => "hour"},
  "rhel"    => {:initial => 102, :usage => 0.066, :unit => "hour"},
  "windows" => {:initial => 140, :usage => 0.011, :unit => "hour"},
  "*"       => {:initial => 115, :usage => 0.028, :unit => "hour"}
},
  "m1.medium" => {
  "unix"    => {:initial => 204, :usage => 0.012, :unit => "hour"},
  "rhel"    => {:initial => 204, :usage => 0.072, :unit => "hour"},
  "windows" => {:initial => 280, :usage => 0.021, :unit => "hour"},
  "*"       => {:initial => 229, :usage => 0.035, :unit => "hour"}
},
  "m1.large"  => {
  "unix"    => {:initial => 443, :usage => 0.037, :unit => "hour"},
  "rhel"    => {:initial => 443, :usage => 0.097, :unit => "hour"},
  "windows" => {:initial => 602, :usage => 0.106, :unit => "hour"},
  "*"       => {:initial => 496, :usage => 0.080, :unit => "hour"}
},
  "m1.xlarge" => {
  "unix"    => {:initial => 886,  :usage => 0.074, :unit => "hour"},
  "rhel"    => {:initial => 886,  :usage => 0.134, :unit => "hour"},
  "windows" => {:initial => 1200, :usage => 0.211, :unit => "hour"},
  "*"       => {:initial => 990,  :usage => 0.139, :unit => "hour"}
}
}

# Cout par demande par métrique
METRICS_COST = {
  "cpu.usage"              => 2.0e-5,
  "mem.usage"              => 2.0e-5,
  "volume.size"            => 1.0e-5,
  "network.incoming.bytes" => 1.0e-5,
  "network.outgoing.bytes" => 1.0e-5,
  "*"                      => 5.0e-5
}

class Cost
  attr_reader :cost

  def initialize(json)
    @data = json
    @resources = json["resource_ids"]
    @period_duration = json["period"]["duration"]
    @period_unit = json["period"]["unit"]
    @monitor_unit = json["metrics"] ? (json["metrics"]["unit"] || "second") : "second"
    @min_cost = -1
    @max_cost = 0
    @avg_cost = 0
    @cost = {"resource_ids" => @resources, "groups" => {}, "cost" => {}}
    calculate()
  end

  private

  def calculate
    cost_list = extract_resources_costs_list
    EBill.info("cost_list => #{cost_list.inspect}")
    @resources.each do |resource_id|
      @cost["groups"][resource_id] = calculate_resource_cost(resource_id, cost_list[resource_id])
    end
    @cost["cost"] = {
      "min" => @min_cost,
      "max" => @max_cost,
      "avg" => @avg_cost
    }
  end

  def extract_resources_costs_list
    cost_list = {}
    @resources.each do |resource_id|
      cost_list[resource_id] = {}
      resource_data = get_resource_data(resource_id)
      ((resource_data["flavor"] && VM_COST[resource_data["flavor"]]) ? {resource_data["flavor"] => VM_COST[resource_data["flavor"]]} : VM_COST).each do |flavor, os|
        cost_list[resource_id][flavor] = (resource_data["os"] && os[resource_data["os"]]) ? [resource_data["os"]] : os.keys
      end
    end
    cost_list
  end

  def calculate_resource_cost(resource_id, cost_list)
    resource_data = get_resource_data(resource_id)
    min = resource_data["min"]
    max = resource_data["max"]

    monitor_cost = (@data["metrics"] && 
                    @data["metrics"]["monitor"] && 
                    @data["metrics"]["monitor"][resource_id]) ?
                    get_monitor_cost(@data["metrics"]["monitor"][resource_id]) : 
                    {"min" => 0, "max" => 0}

    costs = []
    cost_list.each do |flavor, oses|
      oses.each do |os|
        initial_cost = get_initial_cost(flavor, os)
        usage_cost = get_usage_cost(flavor, os)

        total_min = min * (initial_cost + usage_cost + monitor_cost)
        total_max = max * (initial_cost + usage_cost + monitor_cost)

        @min_cost = (@min_cost == -1) ? total_min : [@min_cost, total_min].min
        @max_cost = [@max_cost, total_max].max
        avg = (@min_cost + @max_cost) / 2
        @avg_cost = (@avg_cost == 0) ? avg : (@avg_cost + avg) / 2

        costs << {
          "flavor" => flavor, 
          "os" => os,
          "initial" => {
            "min" => min * initial_cost,
            "max" => max * initial_cost
          },
          "usage" => {
            "min" => min * usage_cost,
            "max" => max * usage_cost
          },
          "monitor" => {
            "min" => min * monitor_cost,
            "max" => max * monitor_cost
          },
          "total" => {
            "min" => min * (initial_cost + usage_cost + monitor_cost),
            "max" => max * (initial_cost + usage_cost + monitor_cost)
          }
        }
      end
    end

    {"min" => min, "max" => max, "costs" => costs}
  end

  def get_initial_cost(flavor, os)
    VM_COST[flavor][os][:initial]
  end

  def get_usage_cost(flavor, os)
    usage_cost = VM_COST[flavor][os][:usage]
    usage_unit = VM_COST[flavor][os][:unit]
    usage_cost * frequence(1, usage_unit, @period_duration, @period_unit)
  end

  def get_monitor_cost(monitors)
    monitor_cost = 0
    monitors.each do |metric, duration|
      monitor_cost += ((METRICS_COST[metric] || METRICS_COST["*"]) * frequence(duration, @monitor_unit, @period_duration, @period_unit))
    end
    monitor_cost
  end

  def get_resource_data(resource_id)
    @data["groups"] ? (@data["groups"][resource_id] || {"min" => 1, "max" => 1}) : {"min" => 1, "max" => 1}
  end

  def to_second(value, unit) 
    EBill.info("to_second(#{value.inspect}, #{unit.inspect})")
    case unit
    when "year"
      365.25 * to_second(value, "day")
    when "month"
      (365.25/12) * to_second(value, "day")
    when "week"
      7 * to_second(value, "day")
    when "day"
      24 * to_second(value, "hour")
    when "hour"
      60 * to_second(value, "minute")
    when "minute"
      60 * value
    when "second"
      value
    else
      -1
    end.to_i
  end

  def frequence(interval_value, interval_unit, period_value, period_unit)
    to_second(period_value, period_unit) / to_second(interval_value, interval_unit) 
  end
end

def cost(data)
  EBill.info("eBill version #{EBill::VERSION}")

  # Collect data (JSON)
  json = EBill.to_json(data)
  data = json[:ok]

  rc = Cost.new(data).cost

  EBill.info("==> #{rc.inspect}")

  EBill.ok(rc)
end
