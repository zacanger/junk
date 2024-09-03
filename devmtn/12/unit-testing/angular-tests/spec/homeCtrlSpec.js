// TDD is all about writing the tests _first_, because we already know what we _want_ to happen,
// before we start writing the app. this is an example.

describe('homeCtrl', function(){
  var $controller
  beforeEach(module('ngTests'))
  beforeEach(inject(function(_$controller_){
    $controller = _$controller_ // wrapping this because ng means lots of the same named crap
  }))
  describe('$scope.returnOne', function(){
    it('returns int 1', function(){ // this is ctrlr _constructor_ that we built up there
      var controller = $controller('homeCtrl', {$scope: {}}) //... with the 'homeCtrl' 'model,' as it were.
      expect(controller.scope.returnOne()).to.equal(1)
    }) // scope is empty obj, because it get its scope
  })
})

// and now, we go write the actual `returnOne()`

