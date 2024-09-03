'use strict'

const
  passport = require('passport')
, Local    = require('passport-local').Strategy
, mongoose = require('mongoose')
, User     = mongoose.model('User')

passport.use(new Local({
    usernameField : 'email'
  , passwordField : 'password'
  },
  (email, password, done) => {
    User.findOne({
      email : email
    }, (findErr, foundUser) => {
      if(findErr){
        done(findErr)
      } else if(foundUser){
        if(foundUser.password === password){
          delete foundUser.password
          done(null, foundUser)
        } else {
          done(null, null, {reason : 'invalid password'})
        }
      } else {
        done(null, null, {reason : 'invalid email'})
      }
    })
  }
))

passport.serializeUser((user, done) => {
  done(null, user)
})
passport.deserializeUser((obj, done) => {
  done(null, obj)
})

module.exports = {
  register(req, res){
    User.create(req.body, (createErr, createResult) => {
      if(createErr){
        res.status(500).json(createErr)
      } else {
        res.json(createResult)
      }
    })
  }
}

