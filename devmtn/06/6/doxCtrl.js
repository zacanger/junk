var dox = require('./dox')

module.exports = {

  getDox: function(req, res){
    dox.find()
      .then(function(response){
        res.send(response)
    })
  },

  addDox: function(req, res){
    new dox(req.body).save(function(err, data){
      if (err){
        res.status(500).send(err)
      } else {
        res.send(data)
      }
    })
  }
}

