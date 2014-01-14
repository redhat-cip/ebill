require 'ebill'

def rate(data)
  EBill.info("eBill version #{EBill::VERSION}")
  EBill.info("Data : #{data.inspect}")

  json = EBill.to_json(data)
  EBill.info("JSON : #{json.inspect}")

  rc = EBill.ok({:total => 1023, :detail => [:some, :data]})
  EBill.info("result : #{rc.inspect}")
  rc
end
