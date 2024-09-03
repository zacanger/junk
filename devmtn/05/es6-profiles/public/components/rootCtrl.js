angular.module('profiles')
.controller('rootCtrl', function($scope, profileSvc){
  profileSvc.getProfiles()
    .then(profiles => $scope.profiles = profiles)
})
