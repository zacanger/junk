'use strict'

var mongoose = require('mongoose')
  , Schema   = mongoose.Schema

var schema = new Schema({
  title         : {type: String, unique: true, required: true, index: true}
, description : {type: String, required: true}
, price       : {type: Number, required: true, min: 0}
})

module.exports = mongoose.model('product', schema)

