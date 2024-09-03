'use strict'

var mongoose = require('mongoose')
  , Schema   = mongoose.Schema

var schema = new Schema({
  products : [{
    item     : {type: Schema.Types.ObjectId, ref: 'product', required: true}
  , quantity : {type: Number, min: 1}
  }]
})

module.exports = mongoose.model('cart', schema)

