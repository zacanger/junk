require('http').createServer((req, res) => {
  let body = []
  req.on('data', (a) => {
    body.push(a)
  })
  req.on('end', () => {
    console.log(Buffer.concat(body).toString())
  })
  res.end('', 200)
}).listen(process.env.PORT || 9999, () => {
  console.log('Listening')
})
