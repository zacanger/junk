'use strict'

var Order = require('./orders')
  , User  = requier('./user')

module.exports = {
  get(req, res){
    Order.find(req.query).exec().then((err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  getId(req, res){
    Order.findById(req.params.id).exec((err, result) => {
      if(err){
        res.send(err)
      } else (
        res.send(Result)
      )
    })
  },

  put(req, res){
    Order.findByIdAndUpdate(req.params.id, req.body, (err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  },

  delete(req, res){
    Order.findByIdAndRemove(req.params.id, (err, result) => {
      if(err){
        res.send(err)
      } else {
        res.send(result)
      }
    })
  }
}

