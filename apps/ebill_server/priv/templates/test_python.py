import ebill


def rate(data):
    ebill.info("eBill version : {0}".format(ebill.VERSION))
    ebill.info("Data: {0}".format(data))

    json = ebill.to_json(data)
    ebill.info("JSON: {0}".format(json))

    rc = ebill.ok({"total": 1023, "detail": ["some", "data"]})
    ebill.info("result: {0}".format(rc))
    return rc
