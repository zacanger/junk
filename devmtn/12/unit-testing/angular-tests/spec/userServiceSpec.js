describe('userSvc', function(){
  var userSvc, $httpBackend // $httpBackend is from angular-mock; it's an... http... backend.
  beforeEach(module('ngTests'))
  beforeEach(inject(function(_userSvc_, _$httpBackend_){
    userSvc      = _userSvc_
    $httpBackend = _$httpBackend_
  }))
  afterEach(function(){
    $httpBackend.verifyNoOutstandingRequest()     // jesus christ, really?
    $httpBackend.verifyNoOutstandingExpectation() // what is with these goddamn methods?
  })
  describe('getUser', function(){
    it('returns user from \'/api/user\'', function(){
      $httpBackend.expectGET('/api/user').respond(200, {name:'z', pass:'zzzz'})
      userSvc.getUser()
      $httpBackend.flush()
    })
  })
})

