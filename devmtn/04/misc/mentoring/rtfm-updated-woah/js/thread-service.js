angular.module('rtfmApp')
.service('threadService', function (firebaseUrl, Firebase) {
  this.getThreads = function () {
    return new Firebase(firebaseUrl.url + '/threads')
  }
  this.getThread = function (threadId) {
    return new Firebase(firebaseUrl.url + '/threads/' + threadId)
  }
  this.getComments = function (threadId) {
    return new Firebase(firebaseUrl.url + '/threads/' + threadId + '/comments')
  }
})
