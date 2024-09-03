var Food = require('../models/Food')
module.exports = {
  getFood: function(req, res){
    Food.find()
      .then(function(response){
        res.send(response)
      })
  },
  
  addFood: function(req, res){
    new Food(req.body).save(function(err, data){
      if (err){
        res.status(500).send(err)
      } else {
        res.send(data)
      }
    })
  },
  
  findCake: function(req, res){
    Food.findById(req.query.id, function(err, food)
      if (err){
        res.status(500).send(err)
      } else {
        res.send(food)
      })
  }
}