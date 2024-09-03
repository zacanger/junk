var express     = require('express')
  , bodyparser  = require('body-parser')
  , session     = ('express-session')
  , port        = 3000
  , app         = express()

app.use(bodyparser.json())

app.use(session({
  secret: 'asdfjkl;;',
  saveUninitialized: true,
  resave: true
}))

app.post('/cart', function(req, res, next){
  if (!req.session){
    req.session.cart = []
  }
  cart.push(req.body)
  res.status(200).json(cart)
})
