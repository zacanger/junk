var site = require('../models/site')

module.exports = {

  create: function(req, res){
    var newSite = new site(req.body)
    newSite.save( function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  },

  read: function(req, res){
    console.log('req.query: ', req.query)
    site.find(req.query)
    .populate('user')
    .exec(function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  },

  update: function(req, res){
    site.findByIdAndUpdate(req.params.id, req.body, function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  },

  delete: function(req, res){
    site.findByIdAndRemove(req.params.id, function(err, result){
      if (err) return res.status(500).send(err)
      res.send(result)
    })
  }
}

