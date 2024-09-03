var app = angular.module('nbaRoutes');

app.service('homeService', function($http, $q, teamService){
  this.getAllData = function() {
    var deferred = $q.defer();
    var vows = {
      mi: teamService.getTeamData('miamiheat'),
      la: teamService.getTeamData('losangeleslakers'),
      snazz: teamService.getTeamData('utahjazz')
    }
    $q.all(vows)
    .then(function(result) {
      deferred.resolve(result);
    });
    return deferred.promise;
  };
});

