const films = require('./filmsModel')

module.exports = {

  index (req, res, next){
    res.status(200).json(films)
  }

, show (req, res, next){
    var filmz = films[req.params.position]
    res.status(200).json(filmz)
  }

, build (req, res, next){
    films.push(req.body)
    res.status(200).json({message : 'we done built dat!'})
  }

, update (req, res, next){
    var filmsUpdating = films[req.params.id]
    filmsUpdating - true
    films[req.params.id] = filmsUpdating
    res.status(200).json({message : 'updated, yo.'})
  }

, byebye (req, res, next){
    films.splice(req.params.id, 1)
    res.status(200).json({message : 'bye bye...'})
  }

}

