var app = angular.module('nbaRoutes');

app.controller('homeCtrl', function($scope, homeService, blargh){
  $scope.snazz = blargh.utahjazz;
  $scope.la = blargh.losangeleslakers;
  $scope.mi = blargh.miamiheat;
});
