var express    = require('express')
  , bodyparser = require('body-parser')
  , cors       = require('cors')
  , dox        = require('./doxCtrl')
  , mongoose   = require('mongoose')
  , mongo      = 'mongodb://127.0.0.1:27017/superfast'
  , app        = express()
  , port       = 9999

app.use(cors())
app.use(bodyparser.json())

app.get('/dox', dox.getDox)
app.post('/dox', dox.addDox)

app.listen(port, function(){
  console.log('up and running on ' + port)
})

mongoose.connect(mongo)
mongoose.connection.once('open', function(){
  console.log('db is being served at ' + mongo)
})

