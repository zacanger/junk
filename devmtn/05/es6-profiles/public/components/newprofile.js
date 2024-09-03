angular.module('profiles')
.directive('newProfile', function(){
  return {
    restrict    : 'E'
  , scope       : {}
  , templateUrl : './newprofile.html'
  , controller($scope, profileSvc){
      $scope.skills = ['']
      $scope.postProfile = function(){
        profileSvc.postProfile($scope.name, $scope.age, $scope.img, $scope.skills)
      }
    }
  }
})
