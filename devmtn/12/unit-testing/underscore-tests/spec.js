// this is... mocha? chai?

// this is where we actually write tests
describe('_.first', function(){
  // this is where we tell it what to put in the markup for this test
  it('be able to return the first element of an array if n is undefined', function(){
    // and this is where we show it how it works
    expect(_.first([1, 2, 3])).to.equal(1)
  })
  it('returns first N elements of array', function(){
    expect(_.first([1, 2, 3, 4, 5])).to.eql(3) // eql is ==, & equal is ===, sorta.
  })
})

// okay, here's a new one.
describe('_.indexOf', function(){
  it('returns the index of the array that matches that value', function(){
    expect(_.indexOf([1, 2, 3], 2)).to.equal(2) // pass in the thing as the second argument
  })
  it('returns a -1 if no value found', function(){
    expect(_.indexOf([1, 2, 3], 9)).to.equal(-1) // multiple expectations
  })
})
