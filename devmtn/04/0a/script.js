var testApp = angular.module('testApp', ['ngRoute']);

// ROUTES

testApp.config(function($routeProvider) {
	$routeProvider
	.when('/', {
		templateUrl : 'pages/home.html',
		controller : 'mainCtrl'
	})
	.when('/about', {
		templateUrl : 'pages/about.html',
		controller : 'aboutCtrl'
	})
	.when('/contact', {
		templateUrl : 'pages/contact.html',
		controller : 'contactCtrl'
	});
});



// CONTROLLERS

testApp.controller('mainCtrl', function($scope) {
	$scope.message = "I'm Zac. What's up.";
});

testApp.controller('aboutCtrl', function($scope) {
	$scope.message = 'One time I made a thing. Another time I musicked.';
});

testApp.controller('contactCtrl', function($scope) {
	$scope.message = 'Or not. lol.';
});

