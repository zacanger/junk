var app = angular.module('itunes');

app.service('itunesService', function($http, $q) {

  this.formatData = function(data) {
    return {
      Track: data.trackName,
      AlbumArt: data.artworkUrl100,
      Artist: data.artistName,
      Preview: data.previewUrl,
      Price: data.trackPrice,
      Genre: data.primaryGenreName,
      Stream: data.radioStationUrl
    }
  }

  this.Search = function(artist) {
    var deferred = $q.defer();

    $http.jsonp('https://itunes.apple.com/search?term=' + artist + '&callback=JSON_CALLBACK')
    .then(function(response) {
      var parsedResponse = response.data.results;
      var artists = [];
      for (var i = 0; i < parsedResponse.length; i++) {
        artists.push(this.formatData(parsedResponse[i]));
      }
      deferred.resolve(artists);
    }.bind(this));

    return deferred.promise;
  }
});
