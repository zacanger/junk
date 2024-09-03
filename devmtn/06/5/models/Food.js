var mongoose = require('mongoose')
  , schema   = mongoose.Schema


var Food = schema({
    name: {type: String, required: true}
  , price: Number
  , gluten: {type: Boolean, required: true}
  , size: {type: String, default: 'Unknown'}
  , nutrition: {
      fat: {type: String, default: "A Lot"}
    , calories: {type: Number, required: true}
    , healthy: {type: Boolean, default: false}
    , deadlyEffects: Array
  }
})

module.exports = mongoose.model('Food', Food)