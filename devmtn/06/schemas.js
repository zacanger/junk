var mongoose = require('mongoose')

// one to one
var oneSchema = mongoose.Schema({
    name: String
  , age: Number
  , car: twoSchema
})
mongoose.model('one', oneSchema)
var twoSchema = mongooseSchema({
  , year: Number
  , make: String
  , model: String
})
mongoose.model('two', twoSchema)

// two-way BREAKS
var oneSchema = mongoose.Schema({
    name: String
  , age: Number
  , car: twoSchema
})
mongoose.model('one', oneSchema)
var twoSchema = mongooseSchema({
  , year: Number
  , make: String
  , model: String
  , driver:  // NO!
})
mongoose.model('two', twoSchema)

// one to many
var oneSchema = mongoose.Schema({
    name: String
  , age: Number
  , cars: [twoSchema]
})
mongoose.model('one', oneSchema)
var twoSchema = mongooseSchema({
  , year: Number
  , make: String
  , model: String
})
mongoose.model('two', twoSchema)

// many to many
var oneSchema = mongoose.Schema({
    name: String
  , age: Number
  , cars: [{mongoose.Types.ObjectId, ref: 'car'}]
})
mongoose.model('one', oneSchema)
var twoSchema = mongooseSchema({
  , year: Number
  , make: String
  , model: String
  , drivers:[{mongoose.Types.ObjectId, ref: 'driver'}]
})
mongoose.model('two', twoSchema)

mongoose.model('car', twoSchema)
mongose.model('driver', oneSchema)

