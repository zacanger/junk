angular.module('eCommerceApp')
.controller('adminCtrl', function ($scope, prodService) {
  $scope.getProds = function () {
    prodService.getProds()
    .then(function (response) {
      $scope.products = response
    })
  }

  $scope.getProds()

  $scope.getProd = function (id) {
    prodService.getProd(id)
    .then(function (response) {
      $scope.product = response
    })
  }

  $scope.createProd = function (createTitle, createPrice, createImg) {
    prodService.createProd(createTitle, createPrice, createImg)
    .then(function (response) {
      $scope.getProds()
    })
  }

  $scope.removeProd = function (id) {
    prodService.removeProd(id)
    .then(function (response) {
      alert('Product Deleted')
      $scope.getProds()
    })
  }

  $scope.updateProd = function (product) {
    var id = product._id
    delete product._id
    delete product.__v
    prodService.updateProd(product, id)
    .then(function (response) {
      $scope.getProds()
    })
  }

})

