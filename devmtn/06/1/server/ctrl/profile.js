var profiles = [
  {
    name: 'Angry McDouche',
    pic: './public/assets/img/angry.jpg',
    status: 'I HATE YOU AND YOUR FACE IS GIVING ME A HEADACHE GODDAMN'
  },{
    name: 'Human Person',
    pic: './public/assets/img/human.jpg',
    status: 'okAYZ but sometimes I ALSO LIKE PUPPIES U NOOOO!'
  },{
    name: 'Meedy Ochre',
    pic: './public/assets/img/meedy.jpg',
    status: 'meh'
  }
]

module.exports = {
  helloPal: function(req, res){
    var friends = function(req, res){
      req.session.currentUser.friends.forEach(function(pal){
        profiles.forEach(function(profile){
          if (pal.name == profile.name){
            friends.push(profile)
          }
        })
      })
      res.send({
        currentUser: req.session.currentUser, friends: friends
      })
    }
  }
}
