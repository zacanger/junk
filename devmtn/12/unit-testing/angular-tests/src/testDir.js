angular.module('ngTests')
.directive('testDir', function(){
  return {
    restrict : 'E'
  , replace  : true
  , template : '<div><h3>howdy</h3></div>'
  }
})

