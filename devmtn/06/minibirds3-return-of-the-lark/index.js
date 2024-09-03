var express     = require('express')
  , bodyparser  = require('body-parser')
  , cors        = require('cors')
  , mongoose    = require('mongoose')
  , siteCtrl    = require('./ctrl/siteCtrl')
  , userCtrl    = require('./ctrl/userCtrl')
  , app         = require('express')
  , port        = 9999
  , mongo       = 'mongodb://127.0.0.1:27017/minibirds3-return-of-the-lark'

app.post('/sighting', siteCtrl.create)
app.get('/sighting', siteCtrl.read)
app.put('/sighting/:id', siteCtrl.update)
app.delete('/sighting/:id', siteCtrl.delete)

app.post('/user', userCtrl.create)
app.get('/user', userCtrl.read)
app.put('/user/:id', userCtrl.update)
app.delete('/user/:id', userCtrl.delete)

mongoose.set('debug', true)
mongoose.connect(mongo)
mongoose.connection.once('open', function(){
  console.log('your db is over at ' + mongo)
})

app.listen(port, function(){
  console.log("...and we're up and active at " + port)
})
