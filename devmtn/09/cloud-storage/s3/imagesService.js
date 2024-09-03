angular.module('myApp')
.factory('imagesService', function($http){
  var service = {}

  service.storeImage = function(imageData, fileName){
    var imageExtension = imageData.split(';')[0].split('/')
    imageExtension = imageExtension[imageExtension.length - 1]

    var newImage = {
      imageName      : fileName
    , imageBody      : imageData
    , imageExtension : imageExtension
    , userEmail      : 'demo@demo.demo'
    }
    return $http.post('/api/newimage', newImage)
  }
  return service
})

