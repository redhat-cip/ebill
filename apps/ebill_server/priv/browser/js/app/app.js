var ebillApp = angular.module('ebillApp', [
    'ui.bootstrap',
    'ngRoute',
    'hljs',
    'angularBootstrapNavTree',
    'glDate',
    'ngAnimate',
    'ebillControllers'
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
    when('/deploy', {
      templateUrl: '/browser/partial/deploy.html',
      controller: 'DeployCtrl'
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
