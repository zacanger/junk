angular.module('miniRouting').controller('ptrl', function($scope, $stateParams, psvc) {

  if ($stateParams.id === 'shoes') {
    $scope.productData = psvc.shoeData;
  }
  if ($stateParams.id === 'socks') {
    $scope.productData = psvc.sockData;
  }

});
