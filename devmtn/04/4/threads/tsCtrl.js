angular.module('rtfm')
  .controller('tsCtrl', function($scope, tSvc, threadsRef, $firebaseArray){
    $scope.threads = $firebaseArray(threadsRef)
    $scope.threads.$loaded().then(function(threads){
      return(threads)
    });
    $scope.createThread = function(username, title){
      $scope.threads.$add({
        username: username,
        title: title
      });
    }
  });

//     event.preventDefault();
//     var username = $scope.user.email;
//     var password = $scope.user.password;

//     loginObj.$login('password', {
//             email: username,
//             password: password
//         })
//         .then(function(user) {
//             console.log('Authentication successful');
//         }, function(error) {
//             console.log('Authentication failure');
//         });

// 	};
// 	var firebaseObj = new Firebase("https://dazzling-torch-2219.firebaseio.com/");
// 	var loginObj = $firebaseSimpleLogin(firebaseObj);

// }]);

