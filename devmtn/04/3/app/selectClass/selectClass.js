angular.module('seatingApp').controller('SelectClassController', function ($scope, classService, $state) {
  $scope.classes = classService.getClass()

  $scope.selectClass = function (idx) {
    classService.selectedClass = classService.getClass()[idx]
    $state.go('view', {
      class: idx
    })
  }
})
