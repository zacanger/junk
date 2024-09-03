var express     = require('express')
  , bodyparser  = require('body-parser')
  , cors        = require('cors')
  , mongoose    = require('mongoose')
  , app         = express()
  , sightCtrl   = require('./ctrl/sightCtrl')
  , port        = 9999
  , mongo       = 'mongodb://127.0.0.1:27017/mini-birds-mongoose'

app.use(bodyparser.json())
app.use(cors())

app.post('/api/sighting', sightCtrl.create)
app.get('/api/sighting', sightCtrl.read)
app.put('/api/sighting/:id', sightCtrl.update)
app.delete('/api/sighting/:id', sightCtrl.delete)

mongoose.set('debug', true)
mongoose.connect(mongo)

mongoose.connection.once('open', function(){
  console.log('your document store is living at ' + mongo)
})

app.listen(port, function(){
  console.log('and we are serving on ' + port)
})
