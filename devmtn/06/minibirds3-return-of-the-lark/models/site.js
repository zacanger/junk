var mongoose = require('mongoose')
var Schema = mongoose.Schema

var siteSchema = new Schema({
    user: {type: Schema.Types.ObjectId, ref: 'user'}
  , bird: [{
      name: {type: String, lowercase: true}
    , order: {type: String, lowercase: true, maxlength: 20}
    , status: {
        type: String
        , lowercase: true
        , enum: [
          , 'extinct'
          , 'extinct in the wild'
          , 'critically endangered'
          , 'endangered'
          , 'vulnerable'
          , 'near threatened'
          , 'conservation dependent'
          , 'least concern'
        ]
      }
    }],
    confirmed: {type: Boolean, default: false}
  , numberSeen: {type: Number, min: 1}
})

module.exports = mongoose.model('site', siteSchema)
