'use strict'

var express     = require('express')
  , cors        = require('cors')
  , bodyparser  = require('body-parser')
  , mongoose    = require('mongoose')
  , session     = require('express-session')
  , passport    = require('passport')
  , mongo       = 'mongodb://127.0.0.1:27017/ecom'
  , app         = express()
  , port        = 9999
  , userCtrl    = require('./server/userCtrl')
  , productCtrl = require('./server/productCtrl')
  , orderCtrl   = require('./server/orderCtrl')
  , passCtrl    = require('./server/passCtrl')

app
.use(cors())
.use(bodyparser.json())
.use(session({secret : 'this is a session secret'}))
.use(passport.initialize())
.use(passport.session())
.use(express.static(__dirname + '/public'))

// .get('/test', (req, res) => {
//   console.log(req.query)
// })

.get('/products', productCtrl.get)
.get('/products/:id', productCtrl.getId)
.post('/products', productCtrl.post)
.put('/products/:id', productCtrl.put)
.delete('/products/:id', productCtrl.delete)

.get('/user', userCtrl.get)
.get('/user/:id', userCtrl.getId)
.post('/user', userCtrl.post)
.put('/user/:id', userCtrl.put)
.delete('/user/:id', userCtrl.delete)
.put('/cart/:id', userCtrl.cart)

.get('/orders', orderCtrl.get)
.get('/orders/:id', orderCtrl.getId)
.post('/orders', orderCtrl.post)
.put('/orders/:id', orderCtrl.put)
.delete('/orders/:id', orderCtrl.delete)


.post('/auth/local/register', passCtrl.post)
.post('/auth/local', passport.authentiate('local'), (req, res) => {
  res.json(req.user)
})

mongoose.connect(mongo, (err) => {
  if(err){
    throw err
  }
  console.log('connecting to mongo')
})
mongoose.connection.once('open', () => {
  console.log('connected to ' + mongo)
})

app.listen(port, () => {
  console.log('express listening on ' + port)
})

