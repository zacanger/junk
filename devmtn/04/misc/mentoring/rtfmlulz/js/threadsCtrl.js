angular.module('rtfmApp')
.controller('threadsCtrl', function($scope, threadService, $firebaseArray){

  $scope.threads = threadService.getThreads()

  $scope.createNewThread = function(newThread){
    threadService.addThread(newThread)
  }

})

