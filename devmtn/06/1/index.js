var express                      = require('express')
  , bodyparser                   = require('body-parser')
  , cors                         = require('cors')
  , session                      = require('express-session')
  , port                         = 9999
  , user                         = require('./server/ctrl/user.js')
  , profile                      = require('./server/ctrl/profile.js')
  , supersecretsessionsecretyeah = require('./server/config.js')
  , app                          = express()
  , corsOptions                  = {
    origin: 'http://127.0.0.1:9999'
  }

app.use(bodyparser.json())
app.use(cors(corsOptions))
app.use(session({secret: supersecretsessionsecretyeah.sessionSecret}))

app.post('/api/login', user.login)
app.get('/api/profiles', profile.helloPal)

app.listen(port, function(){
  console.log('yurp, up at ' + port + 'again, waitin on yerself')
})
