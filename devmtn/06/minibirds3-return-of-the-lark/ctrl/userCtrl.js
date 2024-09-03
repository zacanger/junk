var user = require('../models/user')

module.exports = {

  create: function(req, res){
    var newUserDocument = new user(req.body)
    newUserDocument.save(function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  },

  read: function(req, res){
    user.find(req.query)
    .exec(function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  },

  update: function(req, res){
    user.findByIdAndUpdate(req.params.id, req.body, function(err, result){
        if (err) return res.status(500).send(err)
        res.send(result)
      }
    )
  },

  delete: function(req, res){
    user.findByIdAndRemove(req.params.id, function(err, result){
        if (err) return res.status(500).send(err)
        res.send(result)
      }
    )
  }
}
