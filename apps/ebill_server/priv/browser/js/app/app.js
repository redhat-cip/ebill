var ebillApp = angular.module('ebillApp', [
    'ui.bootstrap',
    'ngRoute',
    'ebillControllers',
    'hljs'
]).run(function($rootScope, $location, $http, $modal) {
  if($location.path() == '') { $location.path('/'); }
  $rootScope.location = $location;

  $rootScope.about = function() {
    var modalInstance = $modal.open({
      templateUrl: '/browser/modal/about.html',
      controller: ModalInstanceCtrl
    });
  };
});

ebillApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
    when('/', {
      templateUrl: '/browser/partial/root.html',
      controller: 'RootCtrl'
    }).
    when('/templates', {
      templateUrl: '/browser/partial/templates.html',
      controller: 'TemplatesCtrl'
    }).
    when('/templates/:language/:name', {
      templateUrl: '/browser/partial/templates.html',
      controller: 'TemplatesCtrl'
    }).
    when('/charging', {
      templateUrl: '/browser/partial/charging.html',
      controller: 'ChargingCtrl'
    }).
    when('/help', {
      templateUrl: '/browser/partial/help.html',
      controller: 'HelpCtrl'
    }).
    otherwise({
      redirectTo: '/'
    });
  }
]);

var ModalInstanceCtrl = function ($scope, $modalInstance) {
  $scope.ok = function () {
    $modalInstance.close();
  };
};
