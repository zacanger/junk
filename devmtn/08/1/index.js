'use strict'

const
  express  = require('express')
, session  = require('express-session')
, passport = require('passport')
, Facebook = require('passport-facebook').Strategy
, app      = express()
, port     = process.env.PORT || 3000

app.use(passport.initialize())
app.use(passport.session())

passport.use(new Facebook({
, callbackURL  : 'http://localhost:3000/auth/facebook/callback'
}, (token, refreshToken, profile, done) => {
  return done(null, profile)
}))

app.get('/auth/facebook', passport.authenticate('facebook'))
app.get('/auth/facebook/callback', passport.authenticate('facebook', {
  successRedirect : '/'
, failureRedirect : '/login'
}), (req, res) => {
  console.log(req.session)
  console.image('http://zacanger.com/gifland/ferret-fail.gif')
})

passport.serializeUser((user, done) => {
  done(null, user)
})

passport.deserializeUser((obj, done) => {
  done(null, obj)
})

app.get('/me', (req, ers) => {
  var currentUser = req.session.user
  res.send(currentUser)
})
app.listen(port, () => {
  console.log('oi oi oi at ' + port)
})

