function rate(data) {
  ebill.info("eBill version " + ebill.VERSION);
  ebill.info("Data : " + data.inspect);

  json = ebill.to_json(data);
  ebill.info("JSON : " + json);

  rc = ebill.ok({"total": 1023, "detail": ["some", "data"]});
  ebill.info("result : " + rc);
  return rc;
}
