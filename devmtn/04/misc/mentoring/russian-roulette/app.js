angular.module('app', ['firebase'])

.controller('ctrl', function($scope, $firebaseObject, $firebaseArray, firebaseSvc){
  var ref = firebaseSvc.getRootRef()
  $scope.game = $firebaseObject(ref.child('game'))
  $scope.pullTrigger = function(){
    var index = $scope.game.currentChamber
    if($scope.game.chambers[index] === true){
      youDied()
    } else {
      incrementChamber()
    }
  }
  $scope.createNewGame = function(){
    $scope.game.chambers = []
    $scope.game.currentChamber = 0
    $scope.game.deadPerson = false
    var bullet = ~~(Math.random()*6)
    for(var i = 0; i < 6; i++){
      if(i === bullet){
        newGameObj.chambers.push(true)
      } else {
        newGameObj.chambers.push(false)
      }
    }
    $scope.game.$save()
  }
  function youDied(){
    alert('BANG!')
    $scope.game.deadPerson = true
    $scope.game.$save()
  }
  function incrementChamber(){
    $scope.game.currentChamber++
    $scope.game.$save()
  }
})

.controller('chat', function($scope, $firebaseObject, $firebaseArray){
  var ref  = firebaseSvc.getRootRef().child('chat')
    , chat = $firebaseArray(ref)
  $scope.chat = chat
  $scope.sendChat = function(){
    $scope.chat.$add({text:$scope.newChat, timestamp: new Date().getTime()})
    $scope.newChat = ''
  }
})

.service('fbSvc', function(){
  this.getRootRef = function(){
    return new Firebase('http://dm7.firebase.io')
  }
})

