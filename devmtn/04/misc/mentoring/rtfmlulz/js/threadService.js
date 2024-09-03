angular.module('rtfmApp')
.service('threadService', function($firebaseObject, $firebaseArray, fb){

  this.addThread = function(newThread) {
    var ref = new Firebase(fb.url + '/threads')
    return $firebaseArray(ref).$add(newThread)
  }

  this.getThreads = function() {
    var ref = new Firebase(fb.url + '/threads')
    return $firebaseArray(ref)
  }

  this.getThread = function(threadId) {
    var ref = new Firebase(fb.url + '/threads/' + threadId)
    return $firebaseObject(ref)
  }

  this.getComments = function(threadId) {
    var ref = new Firebase(fb.url + 'comment/' + threadId)
    return $firebaseArray(ref)
  }

  this.addComment = function(comment, threadId) {
    var ref = new Firebase(fb.url + 'comment/' + threadId)
    return $firebaseArray(ref).$add(comment)
  }

})

