angular.module('ngTests')
.service('userSvc', function($http){
  this.getUser = function(){
    return $http.get('/api/user')
  }
})

