var app = angular.module('itunes');

app.controller('mainCtrl', function($scope, itunesService){
  $scope.gridOptions = {
    data: 'songData',
    height: '120px',
    sortInfo: {fields: ['Track', 'Artist', 'Genre', 'Price'], directions: ['asc', 'desc']},
    columnDefs: [
      {field: 'AlbumArt', displayName: 'Album Art', cellTemplate: '<div class="ngCellText" ng-class="col.colIndex()"><img src="{{row.getProperty(col.field)}}"></div>'},
      {field: 'Track', displayName: 'Track'},
      {field: 'Artist', displayName: 'Artist'},
      {field: 'Genre', displayName: 'Genre'},
      {field: 'Preview', displayName: 'Preview', cellTemplate: '<div class="ngCellText" ng-class="col.colIndex()"><a href="{{ Preview }}">Preview <i class="glyphicon glyphicon-play"></i></a>'},
      {field: 'Stream', displayName: 'Stream', cellTemplate: '<div class="ngCellText" ng-class="col.colIndex()"><a href=" {{Stream}} ">Radio <i class="glyphicon glyphicon-headphones"></i></a>'},
      {field: 'Price', displayName: 'Price'},
    ]
  };

  //Now write a function that will call the method on the itunesService that is responsible for getting the data from iTunes,
  //whenever the user clicks the submit button
  //*remember, that method should be expecting an artist name. The artist name is coming from the input box on index.html,
  //head over there and check if that input box is tied to any specific model we could use.
  //Also note that that method should be retuning a promise, so you could use .then in this function.
  $scope.songData = [];
  $scope.getDat = function() {
    itunesService.Search($scope.artist)
    .then(function(data) {
      $scope.songData = data;
    });
  }

  //Once you have that final data array, you simply need to put it on the scope (or more specifically on the scope as songData). Once you do this ($scope.songData = myFinalArray) then ng-grid will see that and populate the page.

});




