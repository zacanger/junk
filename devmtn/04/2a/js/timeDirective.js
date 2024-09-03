angular.module('timeApp');
app.directive('showTime', function() {
    return {
        restrict: 'A',
        template: '{{time}}',
        link: function(scope, elem, attr) {
            var currentTime = new Date();
            scope.time = currentTime.toString;
        }
    }
})


//angular.module('timeApp');
//app.directive('showTime', ['$interval', 'dateFilter' function($interval, dateFilter) {
  //function updateHOURLY() {
  //element.text(dateFilter(new Date()))
  //scope.$watch(attrs.currentTime, function(value){
  //=value;
  //updateHOURLY();})
  //timeoutId = $interval(function(){
  //updateHOURLY();
  //}, 5);
  //}
    //return {
        //restrict: 'E',
            //template: '<div> The current time is {{time}} </div>',
        //link: function(scope, elem, attr) {
            //var currentTime = new Date();
            //scope.time = currentTime.toString;
        //}
    //}
//}])

