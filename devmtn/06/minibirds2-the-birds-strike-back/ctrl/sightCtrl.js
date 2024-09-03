var sight = require('../models/sight')

module.exports = {

  create: function(req, res){
    var newSight = new sight(req.body)
    newSight.save(function(err, result){
      if (err) return res.status(500).send(err)
      else res.send(result)
    })
  },

  read: function(req, res){
    sight.find(req.query)
    .exec(function(err, result){
      if (err) return res.status(500).send(err)
      else res.send(result)
    })
  },

  update: function(req, res){
    sight.findByIdAndUpdate(req.params.id, req.body, function(err, result){
      if (err) return res.status(500).send(err)
      else res.send(result)
    })
  },

  delete: function(req, res){
    sight.findByIdAndRemove(req.params.id, function(err, result){
      if (err) return res.status(500).send(err)
      else res.send(result)
    })
  }
}
