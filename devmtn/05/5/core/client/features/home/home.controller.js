var app = angular.module('app')

app.controller('homeController', ['$scope', 'friendService', homeController])

function homeController ($scope, friendService) {
  $scope.getFriends = function () {
    friendService.getFriends()
      .then(function (data) {
        $scope.friends = data
      })
  }

  $scope.getFriendById = function (id) {
    friendService.getFriendById(id)
      .then(function (data) {
        $scope.friend = data
      })
  }

  $scope.postNewFriend = function () {
    if (!$scope.name) {
      $scope.flash = 'Please enter a name!'
    }

    else if (!$scope.age || /[^0-9]/.test($scope.age)) {
      $scope.flash = 'Please enter an age (needs to be a number, dummy!)'
    } else {
      var newFriend = {
        name: $scope.name,
        age: $scope.age
      }
      friendService.postNewFriend(newFriend)
        .then(function (data) {
          $scope.flash = data
        })
      $scope.name = ''
      $scope.age = ''
    }

  }
}
