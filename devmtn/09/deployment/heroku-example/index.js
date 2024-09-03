process.env.NODE_ENV = process.env.NODE_ENV || 'development'

var port = process.env.PORT || 3000

var app = require('./server/config/express')()
  , db = require('./server/config/mongoose')()

app.listen(port, function(){
  console.log('listening on ' + port)
})
