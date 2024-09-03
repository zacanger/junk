var app = angular.module('userProfiles')
app.controller('mainCtrl', function ($scope, thatService) {
  $scope.getUsers = function() {
    return thatService.getUsers()
    }
  $scope.users = $scope.getUsers
})

