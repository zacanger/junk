angular.module('carFire')
.controller('carCtrl', function ($scope, $stateParams, carsService) {
  var currentCarId        = $stateParams.carId
  $scope.car              = carsService.getCar(currentCarId)
  $scope.comments         = carsService.getComments(currentCarId)
  $scope.createNewComment = function (comment) {
    carsService.addComment(comment, currentCarId)
  }
})

