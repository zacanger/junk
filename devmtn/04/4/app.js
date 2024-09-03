'use strict';
angular.module('rtfm', [
  'ui.router',
  'firebase'
])
  .constant('fb', {'url': "https://dazzling-torch-2219.firebaseio.com/"})
  .config(['$stateProvider', '$urlRouterProvider', function($stateProvider){
	  $stateProvider
      .state('threads', {
	  	  url: '/threads',
        templateUrl: 'threads/ts.html',
        controller: 'tsCtrl',
        resolve: function(tSvc){
          return tSvc.getThreads()
        }
    })
      .state('thread', {
        url: '/threads/:threadId',
        templateUrl: 'threads/t.html',
        controller: 'tCtrl',
        resolve: {
          threadRef: function(tSvc, $stateParams){
            return tSvc.getThread($stateParams.threadId);
          },
        commentsRef: function(tSvc, $stateParams){
          return tSvc.getComments($stateParams.threadId);
          }
        }
      });
  }]);
