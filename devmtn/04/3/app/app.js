angular.module('seatingApp', ['ui.router']).config(function ($stateProvider) {
  $stateProvider.state('home', {
    controller: 'SelectClassController',
    url: '/home',
    templateUrl: 'app/selectClass/selectTmpl.html'
  }).state('add', {
    controller: 'SetupController',
    url: '/add',
    templateUrl: 'app/setup/setupTmpl.html'
  }).state('view', {
    controller: 'ViewController',
    url: '/view/:class',
    templateUrl: 'app/view/viewTmpl.html'
  })
})
