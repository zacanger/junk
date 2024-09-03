angular.module('seatingApp').controller('SetupController', function ($scope, $state, classService) {
  $scope.donzo = function () {
    classService.addClass({
      name: $scope.className,
      tableCount: $scope.tableCount,
      tableCapacity: $scope.tableCapacity,
      studentCount: $scope.studentCount
    })

    $state.go('home')
  }
})
