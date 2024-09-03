angular.module('uiRouteDemo')

.controller('ctrl', function($scope, $rootScope, $state){
	$scope.things = 'those things'
});

.controller('gone', function($scope, $state){
	$scope.blah = '<button>you should click this button</button>';
	$scope.meh = '<span>BLAHH MEHHH</span>';
});

.controller('settings', function($scope, $state){
	$scope.set = 'Oi';
});

.controller('list', function($scope, listsvc){
	$scope.item = listsvc.list;
})
