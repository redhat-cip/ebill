from erlport.erlterms import Atom
from erlport.erlang import call
import json
import sys


VERSION = "0.0.2"


def info(message):
    call(Atom("error_logger"), Atom("info_msg"), [message])


def error(message):
    call(Atom("error_logger"), Atom("error_msg"), [message])


def warning(message):
    call(Atom("error_logger"), Atom("warning_msg"), [message])


def to_json(data):
    try:
        rc = json.loads(data)
    except ValueError as e:
        rc = {"error": "JSON error #{0}: {1}".format(e.errno, e.strerror)}
    except:
        rc = {"error": "Internal error: {0}".format(sys.exc_info()[0])}
    return rc


def ok(data):
    return json.dumps({"ok": data})


def err(data):
    return json.dumps({"error": data})
