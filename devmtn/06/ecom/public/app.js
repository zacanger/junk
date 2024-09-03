angular.module('eCommerceApp', ['ui.router'])
.config(function($stateProvider, $urlRouterProvider){
  $stateProvider
    .state('products', {
      url: '/products',
      templateUrl: './views/prodTmpl.html',
      controller: 'mainCtrl'
    })
    .state('admin', {
      url: '/admin',
      templateUrl: './views/adminTmpl.html',
      controller: 'adminCtrl'
    })
    .state('login', {
      url: '/login',
      templateUrl: './views/login.html',
      controller: 'loginCtrl'
    })
  $urlRouterProvider.otherwise('/login')
})

