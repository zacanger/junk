angular.module('profiles')
.directive('profile', function(){
  return {
    restrict    : 'E'
  , scope       : {
      profile : '='
    }
  , templateUrl : './profile.html'
  }
})
