angular.module('directives').directive('dweather', function () {
  return {
    scope: {
      currentUser: '=',
      weatherCall: '&'
    },
    templateUrl: 'app/weather/weather.html',
    controller: function ($scope) {
      $scope.$watch('currentUser', function () {
        $scope.weatherCall({city: $scope.currentUser.city})
          .then(function (data) {
            $scope.weather = data.weather
            $scope.temp = data.temperature
          })
      })
    }
  }
})
