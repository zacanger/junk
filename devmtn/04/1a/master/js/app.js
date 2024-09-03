var app = angular.module('miniRouting', ['ui.router'])

app.config(function($stateProvider, $urlRouterProvider) {
  $stateProvider
    .state('home', {
      templateUrl: 'js/home/h.html',
      controller: 'hctrl',
      url: '/'
    })
    .state('settings', {
      templateUrl: 'js/settings/sl.html',
      controller: 'strl',
      url: '/settings'
    })
    .state('products', {
      templateUrl: 'js/products/p.html',
      controller: 'ptrl',
      url: '/products/:id'
    })

  $urlRouterProvider
    .otherwise('/')
})

