angular.module('directives');
app.directive('string', function() {
  scope.user = '=';

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



http://api.openweathermap.org/data/2.5/forecast?q=%s&units=imperial&mode=xml&appid=be22f778633d53500a0fe39dc09ba5d5
