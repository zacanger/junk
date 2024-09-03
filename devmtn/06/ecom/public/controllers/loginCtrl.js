angular.module('eCommerceApp')
.controller('loginCtrl', function ($scope, authService, $state) {
  $scope.loginUser = function (loginInfo) {
    authService.login(loginInfo).then(function (result) {
      $state.go('products')
    })
  }
  $scope.createNewUser = function (userInfo) {
    authService.register(userInfo)
  }
})

