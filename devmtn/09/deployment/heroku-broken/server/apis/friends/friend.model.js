var mongoose = require('mongoose')

var FriendSchema = new mongoose.Schema({
    name: String
  , age: Number
})

module.exports = mongoose.model('Friend', FriendSchema)
