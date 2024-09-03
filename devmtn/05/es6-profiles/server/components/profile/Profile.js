import mongoose from 'mongoose'

const Profile = mongoose.Schema({
  name   : {type: String, required: true}
, age    : {type: Number, required: true}
, img    : {type: String, default: 'http://zacanger.com/logo.png'}
, skills : [String]
, lovejs : {type: Boolean, default: true}
})

export default mongoose.model('Profile', Profile)

