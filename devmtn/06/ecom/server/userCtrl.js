'use strict'

var User = require('./user')

module.exports = {

  post(req, res){
    let newUser = new User(req.body)
    newUser.save((err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  get(req, res){
    User.find(req.query).exec().then((err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  getId(req, res){
    User.findById(req.params.id).exec((err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  put(req, res){
    User.findByIdAndUpdate(req.params.id, req.body, (err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  cart(req, res){
    let products = req.body.products
    for(let i = products.length - 1; i >= 0; i--){
      if(products[i].qty < 1){
        products.splice(i, 1)
      }
    }
    User.findByIdAndUpdate(req.params.id, {cart : {products : products}}, (err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  delete(req, res){
    User.findByIdAndUpdate(req.params.id, (err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  }

}

// schema.pre('save', (next) => {
//   let user = this
//   if(!this.password){
//     return next(new Error('Requires password'))
//   }
//   bcrypt.genSalt(10, (err, salt) => {
//     if(err){
//       return next(new Error(err))
//     }
//     bcrypt.hash(user.password, salt, (err, hash) => {
//       if(err){
//         return next(new Error(err))
//       }
//       user.password = hash
//       next()
//     })
//   })
// })

// schema.methods.checkPw = (pw, cb) => {
//   bcrypt.compare(pw, this.password, (err, res) => {
//     if(err){
//       return cb(err, null)
//     } else {
//       return cb(null, res)
//     }
//   })
// }

