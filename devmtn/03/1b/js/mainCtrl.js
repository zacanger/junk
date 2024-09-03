var app = angular.module('quoteBook';
app.controller('mainCtrl', function($scope datService){
  $scope.addData =
  $scope.getData =
  $scope.removeData =
});




angular.module('quoteBook').controller('mainCtrl', function($scope, mainService) {

	$scope.currentQuotes = mainService.getData();

	$scope.addQuote = function() {
		var newObj = {
			text: $scope.quote,
			author: $scope.author
		};

		mainService.addData(newObj);

		$scope.author = '';
		$scope.quote = '';
	};

	$scope.removeQuote = function(index) {
		mainService.removeData(index);
	};

	$scope.sortOptions = [
		{display: 'Add a Quote' , value: 'authorAddDiv'},
		{display: 'Filter Options' , value: 'filterOptions'}
	];

	$scope.dropDownMenu = 'authorAddDiv';

});
