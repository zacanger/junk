angular.module('timeApp');

app.controller('mainCtrl', function ($scope, timeAgo, nowTime) {
  timeAgo.settings.fullDateAfterSeconds = 60 * 60 * 24;
  $scope.pageLoadTime = (new Date()).toISOString();
  $scope.nowTime = nowTime;
  $scope.nowTimeAsDateObject = new Date();
});