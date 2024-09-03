angular.module('directives').controller('h', function($scope, $http, $q, ws){
  $scope.users = [
 ,  { name: "ryan walsshh"
 ,  age: 42
 ,  email: "stuffsrdelish@junk.li"
 ,  city: "Dresden" }
 ,  { name: "reynaldo naldino"
 ,  age: 12
 ,  email: "mmmmmmmmmgood@junks.in.me"
 ,  city: "Santiago de Cali" }
 ,  { name: "BILLY THE LOUD"
 ,  age: "like 4 or something i guess"
 ,  email: "FUCKemail@SOMUCHRAGE.HATE"
 ,  city: "FUCKCITIESTOO" }
  ];

  function getWeather(city){
    var deferred = $q.defer();
    .then(function(data) {
      deferred.resolve(data);
    });
    return deferred.promise;
  }})
