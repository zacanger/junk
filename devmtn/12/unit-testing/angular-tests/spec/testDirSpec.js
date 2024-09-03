describe('testDir', function(){
  var $compile, $rootScope
  beforeEach(module('ngTests'))
  beforeEach(inject(function(_$compile_, _$rootScope_){
    $compile   = _$compile_
    $rootScope = _$rootScope_
  }))
  it('replaces <test-dir> with some html', function(){
    var element = $compile('<test-dir></test-dir>')($rootScope)
    expect(element.html()).to.equal('<h3>howdy</h3>')
  })
})

