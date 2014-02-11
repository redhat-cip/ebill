angular.module('glDate', ['ui.bootstrap']).directive('dateAfter', function () {
  return {
    require: 'ngModel',
    scope: false,
    controller: function($attrs, $scope) {
      $scope.$watch($attrs.dateAfter, function(value) {
        if(value != undefined && value != "") {
          var _add = eval("("+$attrs.addDateAfter+")");
          minToDate = clone(value).add(_add);
          $scope[$attrs.min] = minToDate;
        }
      });
    },
    link: function (scope, element, attrs, ngModelCtrl) {
      var toDate, minToDate;
      scope.autoMinDate = function() {
        if(minToDate) {
          return minToDate;
        }
      };
      scope.$watch(attrs.dateAfter, function (value) {
        if(value != undefined && value != "") {
          if(attrs.addDateAfter) {
            var _add = eval("("+attrs.addDateAfter+")");
            minToDate = clone(value).add(_add);
          } else {
            minToDate = clone(value);
          }
          validate();
        }
      });
      scope.$watch(attrs.ngModel, function (value) {
        toDate = value;
      });
      function validate() {
        if(toDate != undefined && toDate != "" && toDate < minToDate) {
          ngModelCtrl.$setViewValue(minToDate);
          ngModelCtrl.$render();
        }
      }
    }
  };
});

function clone(obj) {
  // Handle the 3 simple types, and null or undefined
  if (null == obj || "object" != typeof obj) return obj;

  // Handle Date
  if (obj instanceof Date) {
    var copy = new Date();
    copy.setTime(obj.getTime());
    return copy;
  }

  // Handle Array
  if (obj instanceof Array) {
    var copy = [];
    for (var i = 0, len = obj.length; i < len; i++) {
      copy[i] = clone(obj[i]);
    }
    return copy;
  }

  // Handle Object
  if (obj instanceof Object) {
    var copy = {};
    for (var attr in obj) {
      if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
    }
    return copy;
  }

  throw new Error("Unable to copy obj! Its type isn't supported.");
}

(function () {
  var $D = Date,
  $P = $D.prototype,
  p = function (s, l) {
    if (!l) {
      l = 2;
    }
    return ("000" + s).slice(l * -1);
  };

  $P.addDays = function(days) {
    this.setDate(this.getDate() + days);
    return this;
  };

  $P.clearTime = function () {
    this.setHours(0);
    this.setMinutes(0);
    this.setSeconds(0);
    this.setMilliseconds(0);
    return this;
  };

  $D.en = {
    shortDayName: [],
    longDayName: [],
    shortMonthName: [],
    longMonthName: [],
    amDesignator: "AM",
    pmDesignator: "PM",
    timezones: [{name:"UTC", offset:"-000"}, {name:"GMT", offset:"-000"}, {name:"EST", offset:"-0500"}, {name:"EDT", offset:"-0400"}, {name:"CST", offset:"-0600"}, {name:"CDT", offset:"-0500"}, {name:"MST", offset:"-0700"}, {name:"MDT", offset:"-0600"}, {name:"PST", offset:"-0800"}, {name:"PDT", offset:"-0700"}]
  };

  $P.moveToDayOfWeek = function (dayOfWeek, orient) {
    var diff = (dayOfWeek - this.getDay() + 7 * (orient || +1)) % 7;
    return this.addDays((diff === 0) ? diff += 7 * (orient || +1) : diff);
  };

  $P.getOrdinalNumber = function () {
    return Math.ceil((this.clone().clearTime() - new Date(this.getFullYear(), 0, 1)) / 86400000) + 1;
  };

  $P.getTimezone = function () {
    return $D.getTimezoneAbbreviation(this.getUTCOffset());
  };

  $P.getUTCOffset = function () {
    var n = this.getTimezoneOffset() * -10 / 6, r;
    if (n < 0) {
      r = (n - 10000).toString();
      return r.charAt(0) + r.substr(2);
    } else {
      r = (n + 10000).toString();  
      return "+" + r.substr(1);
    }
  };

  $D.getTimezoneAbbreviation = function (offset) {
    var z = $D.en.timezones, p;
    for (var i = 0; i < z.length; i++) {
      if (z[i].offset === offset) {
        return z[i].name;
      }
    }
    return null;
  };

  $D.isLeapYear = function (year) { 
    return (((year % 4 === 0) && (year % 100 !== 0)) || (year % 400 === 0)); 
  };

  $D.getDaysInMonth = function (year, month) {
    return [31, (Date.isLeapYear(year) ? 29 : 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][month];
  };

  $P.isLeapYear = function () { 
    var y = this.getFullYear(); 
    return (((y % 4 === 0) && (y % 100 !== 0)) || (y % 400 === 0)); 
  };

  $P.getDaysInMonth = function () { 
    return Date.getDaysInMonth(this.getFullYear(), this.getMonth());
  };

  $P.addMonths = function (value) {
    var n = this.getDate();
    this.setDate(1);
    this.setMonth(this.getMonth() + value);
    this.setDate(Math.min(n, this.getDaysInMonth()));
    return this;
  };

  $P.addYears = function(value) {
    this.addMonths(value * 12);
    return this;
  };

  $P.add = function(obj) {
    if(obj.days) {
      this.addDays(obj.days);
    }
    if(obj.months) {
      this.addMonths(obj.months);
    }
    if(obj.years) {
      this.addYears(obj.years);
    }
    return this;
  }

  /*
    Format  Description                                                                  Example
    ------  ---------------------------------------------------------------------------  -----------------------
     %a     abbreviated weekday name according to the current localed                    "Mon" through "Sun"
     %A     full weekday name according to the current locale                            "Sunday" through "Saturday"
     %b     abbreviated month name according to the current locale                       "Jan" through "Dec"
     %B     full month name according to the current locale                              "January" through "December"
     %C     century number (the year divided by 100 and truncated to an integer)         "00" to "99"
     %d     day of the month as a decimal number                                         "01" to "31"
     %e     day of the month as a decimal number, a single digit is preceded by a space  "1" to "31"
     %g     like %G, but without the century                                             "08"
     %h     same as %b                                                                   "Jan" through "Dec"
     %H     hour as a decimal number using a 24-hour clock                               "00" to "23"
     %I     hour as a decimal number using a 12-hour clock                               "01" to "12"
     %j     day of the year as a decimal number                                          "001" to "366"
     %m     month as a decimal number                                                    "01" to "12"
     %M     minute as a decimal number                                                   "00" to "59"
     %n     newline character                                                            "\n"
     %p     either "am" or "pm" according to the given time value, or the                "am" or "pm"
            corresponding strings for the current locale
     %S     second as a decimal number                                                   "00" to "59"
     %t     tab character                                                                "\t"
     %u     weekday as a decimal number ["1", "7"], with "1" representing Monday         "1" to "7"
     %U     week number of the current year as a decimal number, starting with the       "0" to ("52" or "53")
            first Sunday as the first day of the first week
     %V     The ISO 8601:1988 week number of the current year as a decimal number,       "00" to ("52" or "53")
            range 01 to 53, where week 1 is the first week that has at least 4 days
            in the current year, and with Monday as the first day of the week.
            (Use %G or %g for the year component that corresponds to the week number
            for the specified timestamp.)
     %W     week number of the current year as a decimal number, starting with the       "00" to ("52" or "53")
            first Monday as the first day of the first week
     %w     day of the week as a decimal, Sunday being "0"                               "0" to "6"
     %y     year as a decimal number without a century                                   "00" "99"
     %Y     year as a decimal number including the century                               "2008"
     %Z     time zone or name or abbreviation                                            "UTC", "EST", "PST"
     %z     same as %Z
     %%     a literal "%" character                                                      "%"
  */
  $P.$format = function (format) {
    var x = this,
        y;

    return format ? format.replace(/(%|\\)?.|%%/g,
        function (m) {
          if (m.charAt(0) === "\\" || m.substring(0, 2) === "%%") {
            return m.replace("\\", "").replace("%%", "%");
          }
          switch (m) {
            case "%d":
              return p(x.getDate());
            case "%a":
              return $D.en.shortDayName[x.getDay()];
            case "%e":
              return x.getDate();
            case "%A":
              return $D.en.longDayName[x.getDay()];
            case "%u":
              return x.getDay() + 1;
            case "%w":
              return x.getDay();
            case "%j":
              return p(x.getOrdinalNumber(), 3);
            case "%U":
              var d1 = x.clone().set({month: 0, day: 1}).addDays(-1).moveToDayOfWeek(0),
                  d2 = x.clone().addDays(1).moveToDayOfWeek(0, -1);
              return (d2 < d1) ? "00" : p((d2.getOrdinalNumber() - d1.getOrdinalNumber()) / 7 + 1);                
            case "%V":
              return x.getISOWeek();
            case "%W":
              return p(x.getWeek());
            case "%B":
              return $D.en.longMonthName[x.getMonth()];
            case "%m":
              return p(x.getMonth() + 1);
            case "%b":
            case "%h":
              return $D.en.shortMonthName[x.getMonth()];
            case "%g":
              return x.$format("%G").slice(-2);
            case "%Y":
              return p(x.getFullYear(), 4);
            case "%y":
              return x.getFullYear();
            case "%p":
              return (x.getHours() < 12 ? $D.en.amDesignator : $D.en.pmDesignator).toLowerCase();
            case "%I":
              return p(x.getHours() < 13 ? (x.getHours() === 0 ? 12 : x.getHours()) : (x.getHours() - 12));
            case "%H":
              return p(x.getHours());
            case "%M":
              return p(x.getMinutes());
            case "%S":
              return p(x.getSeconds());
            case "%z":
            case "%Z":            
              return x.getTimezone();
            case "%C":
              return Math.floor(x.getFullYear() / 100 + 1);
            case "%n":
              return "\\n";
            case "%t":
              return "\\t";
            default:
              return m;
          }
        }
    ) : this.toString();
  };
}());

