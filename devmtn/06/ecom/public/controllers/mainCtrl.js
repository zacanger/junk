angular.module('eCommerceApp')
.controller('mainCtrl', function ($scope, prodService) {
  prodService.getProds()
  .then(function (response) {
    $scope.products = response
  })
  $scope.getProd = function (id) {
    prodService.getProd(id)
    .then(function (response) {
      $scope.product = response
    })
  }
})

