var ebillControllers = angular.module('ebillControllers', ['ui.bootstrap']);

ebillControllers.controller('RootCtrl', function ($rootScope, $scope, $http, $routeParams, $location) {
  $scope.go = function(loc) {
    $location.path(loc);
  };
});
ebillControllers.controller('TemplatesCtrl', function ($scope, $http, $routeParams) {
  $scope.title = "Templates";
  $scope.name = "templates";
  $scope.templates = "";

  $http.get('/template').success(function (data, status, headers, config) {
    if(status == 200) {
      $scope.templates = data;
    }
  });
});
ebillControllers.controller('TemplatesCtrl', function ($scope, $http, $routeParams) {
  $scope.templates = "";

  $http.get('/template').success(function (data, status, headers, config) {
    if(status == 200) {
      $scope.templates = data;
    }
  });

  if($routeParams.language) {
    $http.get('/template/' + $routeParams.name).success(function (data, status, headers, config) {
      $scope.language = $routeParams.language;
      $scope.template = data.template;
      $scope.script = replaceAll("'", "\\'", data.script);
    });
  }
});
ebillControllers.controller('ChargingCtrl', function ($scope, $http, $routeParams) {
  $scope.title = "Charging";
  $scope.name = "charging";
});
ebillControllers.controller('HelpCtrl', function ($scope, $http, $routeParams) {
  $scope.title = "Help";
});

function replaceAll(find, replace, str) {
  return str.replace(new RegExp(find, 'g'), replace);
}
