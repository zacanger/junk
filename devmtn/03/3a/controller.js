var app = angular.module('userProfiles');

app.controller('MainController', function($scope, mainService) {

  $scope.getUsers = function() {
<<<<<<< HEAD:week3/33a/controller.js
  	$scope.users = mainService.getUsers();
  }


=======
    mainService.getUsers().then(function(dataFromService) {
      $scope.users = dataFromService;
    });
  }
>>>>>>> 2eb273603761128b1e61659768dfa9f0267bbe27:controller.js
  $scope.getUsers();
});
