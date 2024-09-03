angular.module('uiRouteDemo', ['ui.router'])


.config(function($urlRouterProvider, $stateProvider) {
	$stateProvider
		.state('home', {
		url: '/',
		templateUrl: 'home.html',
		controller: 'ctrl'
		})
		.state('notHome', {
			url: '/gone',
			template: "<p>{{ blah }}</p><p>{{ meh }}</p>",
			controller: 'gone'
		})
		.state('settings', {
			url: '/settings',
			templateUrl: 'settings.html',
			controller: 'settings'
		})
		.state('list', {
			url: '/list',
			templateUrl: 'list.html',
			controller: 'list'
		})
});
