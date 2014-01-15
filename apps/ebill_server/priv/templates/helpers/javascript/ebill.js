var VERSION="0.0.1";

if (typeof ebill !== 'object') {
  ebill = {};
}

(function () {
  if (typeof ebill.VERSION !== 'string') {
    ebill.VERSION="0.0.1";
  }

  if(typeof ebill.info !== 'function') {
    ebill.info = function (message) {
      // TODO
    };
  }

  if(typeof ebill.error !== 'function') {
    ebill.error = function (message) {
      // TODO
    };
  }

  if(typeof ebill.warning !== 'function') {
    ebill.warning = function (message) {
      // TODO
    };
  }

  if(typeof ebill.to_json !== 'function') {
    ebill.to_json = function (data) {
      return JSON.parse(data);
    }
  }

  if(typeof ebill.ok !== 'function') {
    ebill.ok = function (data) {
      return JSON.stringify({"ok": data});
    };
  }

  if(typeof ebill.err !== 'function') {
    ebill.err = function (data) {
      return JSON.stringify({"error": data});
    };
  }
}());

