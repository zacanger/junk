const
  AWS     = require('aws-sdk')
, Keys    = require('./keys.js')
, s3      = new AWS.S3()

let exports = module.exports = {}

AWS.config.update({
  accessKeyId     : Keys.amazonAccess
, secretAccessKey : Keys.amazonSecret
, region          : Keys.amazonRegion
})


exports.saveImage = (req, res) => {
  let buf = new Buffer(req.body.imageBody.replace(/^data:image\/\w+;base64,/, ''), 'base64')

  // bucketName var below crates a "directory" for each user
  let bucketName = 'demo/' + req.body.userEmail
  let params = {
      Bucket      : bucketName
    , Key         : req.body.imageName
    , Body        : buf
    , ContentType : 'image/' + req.body.imageExtension
    , ACL         : 'public-read'
  }

  s3.upload(params, (err, data) => {
    console.log(err, data)
    if (err) {
      return res.status(500).send(err)
    }
    // this is where we'd actually deal with this data, save the metadata to mongo, etc.
    res.json(data)
  })
}

