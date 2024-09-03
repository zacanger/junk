var routerApp = angular.module('routerApp', ['ui.router']);

routerApp.config(function($stateProvider, $urlRouterProvider) {
	$urlRouterProvider.otherwise('/home');
	$stateProvider


	.state('home', {
		url: '/home',
		templateUrl: 'home.html'
	})

	.state('home.list', {
		url: '/list',
		templateUrl: 'list.html',
		controller: function($scope) {
			$scope.things = ['computers', 'guitars', 'wasted years'];
		}
	})

	.state('home.paragraph', {
		url: '/paragraph',
		template: "Geeezuss, it's late."
	})

	.state('about', {
	url: '/about',
	views: {
		'': {templateUrl: 'about.html' },
		'colOne@about': { template: "hey, ho! (let's go...?)" },
		'colTwo@about': {
			templateUrl: 'data.html',
			controller: 'whatCtrl'
		}
	});
}});


routerApp.controller('whatCtrl', function($scope) {
	$scope.message = 'boing';
	$scope.stuffs = [
		{ name: 'whatever', thingy: 'some' },
		{ name: 'blargh', thingy: 'ohhkay' }
	]
});



