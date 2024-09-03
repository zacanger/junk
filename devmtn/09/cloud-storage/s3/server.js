const
  express    = require('express')
, bodyParser = require('body-parser')
, cors       = require('cors')
, mongoose   = require('mongoose')
, app        = express()
, port       = process.env.PORT || 3000
, Controller = require('./mainController.js')
, mongooseUri = 'mongodb://127.0.0.1:27017/s3'

mongoose.connect(mongooseUri)

let db = mongoose.connection
db.on('error', console.error.bind(console, 'connection error:'))
db.once('open', callback => {
  console.log('mongoose is over at ', mongooseUri)
})

app
.use(express.static(__dirname + '/'))
.use(cors())
.use(bodyParser.json({limit : '50mb'}))
.use(bodyParser.urlencoded({limit : '50mb', extended : true}))
.post('/api/newimage', Controller.saveImage)
.listen(port, () => {
  console.log(`listening on ${port}`)
})

