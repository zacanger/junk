var Ctrl = require('./friend.controller')

module.exports = function(app){
  app.route('/api/friends')
  .post(Ctrl.postFriend)
  .get(Ctrl.getFriends)
}
