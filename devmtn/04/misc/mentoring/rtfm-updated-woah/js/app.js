angular.module('rtfmApp', ['firebase', 'ui.router'])

.constant('firebaseUrl', {
  url: 'http://dm7.firebaseio.com'
})

.config(function ($stateProvider, $urlRouterProvider) {
  $stateProvider
    .state('threads', {
      url: '/threads',
      controller: 'threadsCtrl',
      templateUrl: '/templates/threads.html',
      resolve: {
        threadsRef: function (threadService) {
          return threadService.getThreads()
        }
      }
    })
    .state('thread', {
      url: '/threads/:threadId',
      controller: 'threadCtrl',
      templateUrl: '/templates/thread.html',
      resolve: {
        threadRef: function (threadService, $stateParams) {
          return threadService.getThread($stateParams.threadId)
        },
        commentsRef: function (threadService, $stateParams) {
          return threadService.getComments($stateParams.threadId)
        }
      }
    })

  $urlRouterProvider.otherwise('/threads')

})

