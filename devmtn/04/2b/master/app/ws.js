angular.module('directives').service('ws', function($http, $q) {
  this.getWeather = function(city) {
    var deferred = $q.defer();
    $http.get({"http://api.openweathermap.org/data/2.5/forecast?q=" + city + "&units=imperial&mode=json&appid=be22f778633d53500a0fe39dc09ba5d5"});
    .then(function(data) {
      var results = data.data.results;
      deferred.resolve(results);
    });
    return deferred.promise;
  }
});
