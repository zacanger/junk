let frank = {
  name     : 'frank'
, location : 'somewhere'
, hobbies  : [{
    name : 'stuff'
  , type : 'current'
},{
    name : 'things'
  , type : 'past'
}]
, occupations : [{
    name : 'pizza guy'
  , type : 'current'
  }]
}


module.exports = {
  addHeaders (req, res, next) {
    res.status(200).set({
      'Content-Type'                 : 'application/json'
    , 'Access-Control-Allow-Origin'  : '*'
    , 'Access-Control-Allow-Methods' : 'OPTIONS, GET, POST'
    , 'Access-Control-Allow-Headers' : 'Origin, X-Requested-With, Content-Type, Accept'
    , 'X-XSS-Protection'             : '1; mode=block'
    , 'X-Frame-Options'              : 'SAMEORIGIN'
    , 'Content-Security-Policy'      : "default-src 'self'"
    })
    next()
  }
, generateId (req, res, next) {
    req.body.skill.id = req.body.skills.length
    next()
  }
}

