module.exports = {
  getName (req, res) {
    res.send({name : name})
  }
, getLocation (req, res) {
    res.send({location : location})
  }
, getOccupations (req, res) {
    if (!req.query.order) {
      res.send({occupations : occupations})
    }
    if (req.query.order === 'asc') {
      res.send({occupations : occupations.sort()})
    } else if (req.query.order === 'desc') {
      res.send({occupations : occupations.reverse()})
    }
  }
, latestOccupation (req, res) {
    res.send({latestOccupation : occupations[occupations.length - 1]})
  }
, getHobbies (req, res) {
    res.send({hobbies : hobbies})
  }
, getHobbiesByType (req, res) {
    var hobbiesByType = []
    hobbies.forEach(function (hobby) {
      if (hobby.type === req.params.type) {
        hobbiesByType.push(hobby)
      }
    })
    res.send({hobbiesByType : hobbiesByType})
  }
, getSkillz (req, res) {
    if (req.query.experience) {
      var skillzByExperience = []
      skillz.forEach(function (skill) {
        if (skill.experience === req.query.experience) {
          skillzByExperience.push(skill)
        }
      })
      res.send({skillz : skillzByExperience})
    }
    res.send({skillz : skillz})
  }
, changeName (req, res) {
    name = req.body.name
    res.send({name : name})
  }
, changeLocation (req, res) {
    location = req.body.location
    req.send({location : location})
  }
, addOccupation (req, res) {
    occupations.push(req.body.occupation)
    res.send({occupations : occupations})
  }
, addHobby (req, res) {
    hobbies.push(req.body.hobby)
    res.send({hobbies : hobbies})
  }
}

