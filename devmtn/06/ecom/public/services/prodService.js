angular.module('eCommerceApp')
.service('prodService', function ($http) {
  this.getProds = function () {
    return $http({
      method: 'GET',
      url: 'http://localhost:3000/products'
    }).then(function (response) {
      return response.data
    })
  }
  this.getProd = function (id) {
    return $http({
      method: 'GET',
      url: 'http://localhost:3000/products/' + id
    }).then(function (response) {
      return response.data
    })
  }
  this.createProd = function (createTitle, createPrice, createImg) {
    return $http({
      method: 'POST',
      url: 'http://localhost:3000/products/',
      data: {
        title: createTitle,
        price: createPrice,
        img: createImg
      }
    }).then(function (response) {
      return response.data
    })
  }
  this.removeProd = function (id) {
    return $http({
      method: 'DELETE',
      url: 'http://localhost:3000/products/' + id
    }).then(function (response) {
      return response.data
    })
  }
  this.updateProd = function (product, id) {
    return $http({
      method: 'PUT',
      url: 'http://localhost:3000/products/' + id,
      data: product
    }).then(function (response) {
      return response.data
    })
  }
})

