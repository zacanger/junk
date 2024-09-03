var app = angular.module('nbaRoutes', ['ui.route']);

app.config(function($stateProvider, $httpProvider) {
  $httpProvider.interceptors.push('httpRequestInterceptor');

  $stateProvider
    .state('/', {
      templateUrl: 'js/home/homeTmpl.html',
      controller: 'homeCtrl',
      resolve: {
        blargh: function($route, homeService) {
          return homeService.getAllData();
        }
      }
    })
    .state('/teams/:team', {
      templateUrl: 'js/teams/teamTmpl.html',
      controller: 'teamCtrl',
      resolve: {
        teamData: function($route, teamService) {
          return teamService.getTeamData($route.current.params.team);
        }
      }
    })
    .otherwise({
      redirectTo: '/'
    });
});


