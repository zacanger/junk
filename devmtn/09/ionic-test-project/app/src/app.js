'use strict'

angular.module('IonicTest', [
  'ionic',
  'ngCordova',
  'ngResource'
])
.run( [
  '$ionicPlatform',

  function($ionicPlatform){
  $ionicPlatform.ready(function(){
    // save to use plugins here
  })
  // add possible global event handlers here
}])

.config([
  '$httpProvider',
  '$stateProvider',
  '$urlRouterProvider',

  function($httpProvider, $stateProvider, $urlRouterProvider){
    $stateProvider
      .state('app', {
        url: '/app',
        abstract: true,
        templateUrl: 'templates/main.html',
        controller: 'MainController'
      })
      .state('app.home', {
        url: '/home',
        cache: true,
        views: {
          'viewContent': {
            templateUrl: 'templates/views/home.html',
            controller: 'HomeController'
          }
        }
      })
      .state('app.settings', {
        url: '/settings',
        cache: true,
        views: {
          'viewContent': {
            templateUrl: 'templates/views/settings.html',
            controller: 'SettingsController'
          }
        }
      })
    $urlRouterProvider.otherwise('/app/home');
  }
])

.controller('MainController', require('./controllers/mainController'))
.controller('HomeController', require('./controllers/homeController'))
.controller('SettingsController', require('./controllers/settingsController'))

.factory('ExampleService', require('./services/ExampleService'))
.factory('ApiService', require('./services/ApiService'))
;
