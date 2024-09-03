'use strict'

const
  express    = require('express')
, bodyparser = require('body-parser')
, midware    = require('./ctrl/midware.js')
, mainCtrl   = require('./ctrl/main.js')
, port       = 9999
, app        = express()

app
.use(bodyparser.json())
.use(midware.addHeaders)

.get('/name', mainCtrl.getName)
.get('/location', mainCtrl.getLocation)
.get('/occupations', mainCtrl.getOccupations)
.get('/occupations/latest', mainCtrl.latestOccupation)
.get('/hobbies', mainCtrl.getHobbies)
.get('/hobbies/:type', mainCtrl.getHobbiesByType)
.get('/skillz', mainCtrl.getSkillz)

.put('/name', mainCtrl.changeName)
.put('/location', mainCtrl.changeLocation)
.post('/occupations', mainCtrl.addOccupation)
.post('/hobbies', mainCtrl.addHobby)
.post('/skillz', midware.generateId, mainCtrl.addSkill)

.listen(port, () => console.log(`howdy, imma hang out on ${port}`))


