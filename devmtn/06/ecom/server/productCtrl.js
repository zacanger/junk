'use strict'

var Product = require('./product')

var handleGet = function(req, res){
  Product.find(req.query, function(err, response){
    if(err) {
      res.status(500).json(err)
    } else {
      res.json(response)
    }
  })
}

var handleGetId = function(req, res){
  Product.findById(req.params.id)
  .exec(function(err, result){
    if(err){
      res.send(err)
    } else {
      res.send(result)
    }
  })
}

var handlePost = function(req, res){
  Product.create(req.body, function(error, response){
    if(error) {
      return res.status(500).json(error)
    } else {
      return res.json(response)
    }
  })
}

var handlePut = function(req, res){
  Product.update(req.query, req.body, function(error, response){
    if(error) {
      return res.status(500).json(error)
    } else {
      return res.json(response)
    }
  });
}

var handleDelete = function(req, res){
  Product.remove(req.query, function(error, response){
    if(error) {
      return res.status(500).json(error)
    } else {
      return res.json(response)
    }
  })
}

module.exports = {
  get    : handleGet
, getId  : handleGetId
, post   : handlePost
, put    : handlePut
, delete : handleDelete
}

