angular.module('seatingApp').controller('ViewController', function ($scope, classService, $stateParams) {
  $scope.selectedClass = classService.getClass()[$stateParams.class / 1]
})
