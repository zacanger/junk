import express    from 'express'
import mongoose   from 'mongoose'
import bodyparser from 'body-parser'
import cors       from 'cors'

const
  app   = express()
, port  = process.env.PORT || 9999
, mongo = 'mongodb://127.0.0.1:27017/es6'

mongoose.connect(mongo)
mongoose.connection(once('open', () => console.log(`connected to mongo at ${mongo}`)))

app
.use(bodyparser.json())
.use(cors())
.use(express.static(__dirname, '/dist'))
.listen(port, () => console.log(`listening on ${port}`))

