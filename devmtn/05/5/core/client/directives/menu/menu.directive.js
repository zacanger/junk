var app = angular.module('app')

app.directive('menu', function () {
  return {
    restrict: 'E',

    templateUrl: 'directives/menu/menu.template.html',

    controller: function ($scope) {
      $scope.menu = 'menu'
    }
  }
})
