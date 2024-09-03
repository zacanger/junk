(function(angular) {
  'use strict';
angular.module('OHSORANDOM', [])
  .controller('cuntroller', ['$scope', function($scope) {
    $scope.wut = { query: 'whaaaaaaaaaat', answer: 'YOUR MOTHER, OBVIOUSLY.' };
    $scope.lol = { query: 'why', answer: 'BECAUSE I SAID SO' };
    $scope.onClick = function(){
      $scope.lol.answer = $.append(' , bitch.');
    };
    
  }])
  
  
  
  
  .directive('stupidity', function() {
    return {
      restrict: 'E',
      scope: {
        borky: '=clusterforked',
        title: '@whatthewhat',
        clickT: '&cLicky'
      },
      templateUrl: 'my-customer-iso.html',
      link: function(scope, elem, attr){
        elem.on('click', function(){
          scope.clickT();
          scope.$apply();
        });
      }
    };
  });
})(window.angular);
