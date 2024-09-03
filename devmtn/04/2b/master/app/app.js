var app = angular.module('directives', ['ui.router'])
app.config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
  $stateProvider
    .state('home', {
      url: '/',
      templateUrl: 'app/home/home.html',
      controller: 'homeCtrl'
    })
  $urlRouterProvider.otherwise('/')
}])
