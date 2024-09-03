var app = angular.module('dox', ['ngRoute'])

.config(function($routeProvider){
  $routeProider
    .when('/', {
      template: '<ul><li ng-repeat="doc in dox"><h3>{{doc.name}}</h3><br><em>{{doc.date}}</em><br><strong><small>{{doc.format}} &mdash; {{doc.size}} &mdash; {{doc.license}}</small></strong></li></ul>',
    controller: 'doxCtrl'
})

.controller('doxCtrl', function($scope, $routeParams){
  $scope.name = 'doxCtrl'
  $scope.params = $routeParams
})

