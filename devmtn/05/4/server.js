'use strict'

const
  express     = require('express')
, bodyparser  = require('body-parser')
, app         = express()
, port        = 9999
, usersCtrl   = require('./usersCtrl')
, filmsCtrl   = require('./filmsCtrl')

app
.use(bodyparser())

.get('/users', usersCtrl.index)
.get('/users/:id', usersCtrl.show)
.post('/users', usersCtrl.build)
.put('/users/:id', usersCtrl.update)
.delete('/users/:id', usersCtrl.byebye)

.get('/films', filmsCtrl.index)
.get('/films/:id', filmsCtrl.show)
.post('/films', filmsCtrl.build)
.put('/films/:id', filmsCtrl.update)
.delete('/films/:id', filmsCtrl.byebye)

.listen(port, () => console.log(`listening on ${port}!`))

