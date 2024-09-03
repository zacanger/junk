module.exports = {
    db: 'mongodb://127.0.0.1:27017/deploytest'
  , sessionSecret: "developmentSecret"
  , google: {
      clientID: "CLIENTID"
    ,   clientSecret: "CLIENTSECRET"
    ,   callbackURL: "http://localhost:3000/auth/callback"
    }
}
