angular.module('directives');
app.directive('string', function() {
  return {
    restrict: 'EA',
    templateUrl: 'dir.html',
    link: function(scope, elem, attr) {
      elem.on('click', function(){
      ng-show = !ng-show;
      $scope.apply;
      }
    }
  }
})

