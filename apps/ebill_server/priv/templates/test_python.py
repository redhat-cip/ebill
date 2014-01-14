import json
import version


def rate(data):
    print("eBill version " + version.EBILL_VERSION)
    print("data received : ")
    print(data)
    json.loads(data)
    print("return 200")
    # Do nothing with data
    return 200
