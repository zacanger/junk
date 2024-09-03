angular.module('eCommerceApp')
.service('authService', function ($http) {
  this.register = function (registerInfo) {
    console.log(registerInfo)
    return $http({
      method: 'POST',
      url: '/auth/local/register',
      data: registerInfo
    }).then(function (response) {
      return response.data
    })
  }
  this.login = function (loginInfo) {
    return $http({
      method: 'POST',
      url: '/auth/local',
      data: loginInfo
    }).then(function (response) {
      return response.data
    })
  }
})

