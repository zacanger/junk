var express = require('express')
  , session = require('express-session')
  , app = express()
	, bodyparser = require('body-parser')
  , UsersCtrl = require ('./controllers/UsersCtrl')
  , MoviesCtrl = require('./controllers/MoviesCtrl')
  , cors = require('cors')
  , port = 3000

app.use(bodyparser.json())

app.use(session({
  secret: 'asdfjkl;;',
  saveUninitialized: true,
  resave: true
}))

app.get('/users', UsersCtrl.index)
app.get('/users/:id', UsersCtrl.show)
app.post('/users', UsersCtrl.build)
app.put('/users/:id', UsersCtrl.update)
app.delete('/users/:id', UsersCtrl.destroy)

app.post('/cart', function(req, res, next){
	req.session.cart.push(req.body)
	res.status(200).json(req.session.cart)
})

// app.get('/movies', MoviesCtrl.index)
// app.get('/movies/:id', MoviesCtrl.show)
// app.post('/movies', MoviesCtrl.build)
// app.put('/movies/:id', MoviesCtrl.update)
// app.delete('/movies/:id', MoviesCtrl.destroy)

app.listen(port, function(){
  console.log('welcome, master. ' +  port + ' is ready for you.')
})
