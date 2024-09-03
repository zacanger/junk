var users = [
  {
    name: 'Angry McDouche',
    pass: 'asdfghjkl9',
    friends: ''
  },{
    name: 'Human Person',
    pass: 'IREALLYLIKEKITTENS',
    friends: ['ANYONE', 'KITTENS']
  },{
    name: 'Meedy Ochre',
    pass: 'password1',
    friends: ['Your Mother', 'No not really, sorry', 'my mother']
  }
]

module.exports = {
  login: function(req, res){
		users.forEach(function(user){
			if (user.userName === req.body.name && user.pass === req.body.pass){
        req.session.currentUser = user
        res.send({userFound: true})
        return
      }
    })
    res.send({userFound: false})
  }
}



