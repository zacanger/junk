var User = require('../models/User')

module.exports = {
  index: function(req, res, next) {
    res.status(200).json(User)
  },
  show: function(req, res, next) {
    var requestedUser = User[req.params.id]
    res.status(200).json(requestedUser)
  },
  build: function(req, res, next) {
    User.push(req.body)
    res.status(200).json({ message: 'cool beans' })
  },
  update: function(req, res, next) {
    var userToUpdate = User[req.params.id]
    userToUpdate.updated = true
    User[req.params.id] = userToUpdate
    res.status(200).json({ message: 'updated' })
  },
  destroy: function(req, res, next) {
    User.splice(req.params.id, 1)
    res.status(200).json({ message: 'you no longer exist' })
  }
}

