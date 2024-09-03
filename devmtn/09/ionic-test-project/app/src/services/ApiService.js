'use strict'

module.exports = [
    '$window',
    '$http',
    'API_ENDPOINT',

    function($window, $http, API_ENDPOINT){
      var _api = API_ENDPOINT
      var endpoint = _api.port ? (_api.host + ':' + _api.port + _api.path) : (_api.host + _api.path)

      if (_api.needsAuth) {
        $http.defaults.headers.common.Authorization = 'Basic ' + $window.btoa(_api.username + ':' + _api.password)
      }
      return {
        getEndpoint: function(){return endpoint}
      }
    }
]
