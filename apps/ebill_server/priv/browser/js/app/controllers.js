var ebillControllers = angular.module('ebillControllers', ['ui.bootstrap']);

ebillControllers.controller('RootCtrl', function ($rootScope, $scope, $http, $routeParams, $location) {
  $scope.go = function(loc) {
    $location.path(loc);
  };
});
ebillControllers.controller('TemplatesCtrl', function ($scope, $http, $routeParams, $parse) {
  $scope.templates = [];
  $http.get('/template').success(function (data, status, headers, config) {
    if(status == 200) {
      for(language in data) {
        $scope.templates.push({label: language, children: data[language]});
      }
    }
  });

  $scope.my_tree_handler = function(branch) { 
    $http.get('/template/' + branch.label, {cache: false}).success(function (data, status, headers, config) {
      $scope.template = data.template;
      $scope.script = data.script; 
    });
  };

  $scope.show_highlight = function() {
    return $scope.script;
  }
});
ebillControllers.controller('ChargingCtrl', function ($scope, $http, $routeParams) {
  $scope.templates = [];
  $http.get('/template').success(function (data, status, headers, config) {
    if(status == 200) {
      for(language in data) {
        $scope.templates.push({label: language, children: data[language]});
      }
    }
  });

  $scope.my_tree_handler = function(branch) { 
    $http.get('/template/' + branch.label, {cache: false}).success(function (data, status, headers, config) {
      $scope.template = data.template;
    })
  };

  $scope.open_start_date = function($event) {
    $event.preventDefault();
    $event.stopPropagation();

    $scope.start_date_opened = true;
  };
  $scope.open_end_date = function($event) {
    $event.preventDefault();
    $event.stopPropagation();

    $scope.end_date_opened = true;
  };
  $scope.dateOptions = {
    'year-format': "'yy'",
    'starting-day': 1
  };

  $scope.execute = function(project_id, ressource_id, start_date, end_date, metrics) {
    $scope.alerts= [];
    $scope.result = "";
    if($scope.template == undefined) {
      $scope.alerts.push({type: 'danger', msg: "You must choose a template"});
      return;
    }
    if(project_id == undefined || project_id == "") {
      $scope.alerts.push({type: 'danger', msg: "You must enter a Project ID"});
      return;
    }
     
    $scope.request = {"project_id": project_id, "template": $scope.template};
    if(ressource_id != undefined && ressource_id != "") {
      $scope.request["resource_id"] = ressource_id
    }
    period = {};
    if(start_date != undefined && start_date != "") {
      period["start_date"] = start_date.$format("%Y-%m-%d");
    }
    if(end_date != undefined && end_date != "") {
      period["end_date"] = end_date.$format("%Y-%m-%d");
    }
    if(!isEmpty(period)) {
      $scope.request["period"] = period
    }
    if(!isEmpty(metrics)) {
      var reg = new RegExp("[ ,;]+", "g");
      $scope.request["metrics"] = metrics.split(reg);
    }

    $http({
      url: '/charging',
      method: "POST",
      data: $scope.request,
      headers: {'Content-Type': 'application/json'}
    }).success(function(data, status, headers, config) {
      $scope.result = data;
    }).error(function(data, status, headers, config) {
      $scope.alerts.push({type: 'danger', msg: "Server error #" + status});
    });
  };

  $scope.closeAlert = function(index) {
    $scope.alerts.splice(index, 1);
  };

  $scope.show_result = function() {
    return JSON.stringify($scope.result, null, 2);
  };

  $scope.show_request = function() {
    return JSON.stringify($scope.request, null, 2);
  }
});
ebillControllers.controller('DeployCtrl', function ($scope, $http, $routeParams, $parse) {
  $scope.templates = [];
  $http.get('/template').success(function (data, status, headers, config) {
    if(status == 200) {
      for(language in data) {
        $scope.templates.push({label: language, children: data[language]});
      }
    }
  });

  $scope.my_tree_handler = function(branch) { 
    $http.get('/template/' + branch.label, {cache: false}).success(function (data, status, headers, config) {
      $scope.template = data.template;
      $scope.script = data.script; 
    });
  };
});
ebillControllers.controller('HelpCtrl', function ($scope, $http, $routeParams) {
  $scope.title = "Help";
});


// Speed up calls to hasOwnProperty
var hasOwnProperty = Object.prototype.hasOwnProperty;

function isEmpty(obj) {
  // null and undefined are "empty"
  if (obj == null) return true;

  // Assume if it has a length property with a non-zero value
  // that that property is correct.
  if (obj.length > 0)    return false;
  if (obj.length === 0)  return true;

  // Otherwise, does it have any properties of its own?
  // Note that this doesn't handle
  // toString and valueOf enumeration bugs in IE < 9
  for (var key in obj) {
      if (hasOwnProperty.call(obj, key)) return false;
  }

  return true;
}
