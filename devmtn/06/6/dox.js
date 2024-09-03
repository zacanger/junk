var mongoose    = require('mongoose')
  , schema      = mongoose.Schema

var dox = schema({
    format: {type: String, required: true}
  , name: {type: String, required: true}
  , size: Number
  , modified: {type: Date, default: new Date()}
  , contributors: [{
      name: String
    , edits: Number
    , lastMod: Number
    }]
  , license: String
})

module.exports = mongoose.model('dox', dox)
