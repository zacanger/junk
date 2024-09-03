var express    = require('express')
  , app        = express()
  , bodyparser = require('body-parser')
  , bcrypt     = require('bcrypt')
  , cors       = require('cors')
  , mongoose   = require('mongoose')
  , foodCtrl   = require('./ctrl/foodCtrl')
  , port       = 9999
  , mongo      = 'mongodb://127.0.0.1/menuTwo'

app.use(bodyparser.json())
app.use(cors())
app.use(express.static(__dirname + '/public'))

mongoose.connect(mongo)
mongoose.connection.once('open', function(){
  console.log('connected to ' + mongo)
})

app.get('/api/food', foodCtrl.getFood)
app.get('/api/food/cake', foodCtrl.getCake)
app.post('api/food', foodCtrl.addFood)