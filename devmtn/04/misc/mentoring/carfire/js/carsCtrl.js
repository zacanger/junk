angular.module('carFire')
.controller('carsCtrl', function ($scope, carsService) {
  $scope.cars         = carsService.getCars()
  $scope.createNewCar = function (newCar) {
    carsService.addCar(newCar)
  }
})

