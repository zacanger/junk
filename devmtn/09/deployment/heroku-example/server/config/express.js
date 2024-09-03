var config     = require('./config')
  , express    = require('express')
  , bodyParser = require('body-parser')
  , session    = require('express-session')
  , morgan     = require('morgan')
  , app        = express()

module.exports = function(){

  app.use(bodyParser.json())
  app.use(bodyParser.urlencoded({ extended: true }))
  app.use(morgan('dev'))

  app.use(session({
    saveUninitialized: true
  , resave: true
  , secret: config.sessionSecret
    }))

  require('../apis/friends/friend.routes')(app)

  app.use(express.static('./public'))
    return app
}
