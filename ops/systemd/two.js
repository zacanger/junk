const
  http = require('http')
, port = process.env.PORT || 9900
, host = process.env.HOST || '127.0.0.1'

http.createServer((req, res) => {
  res.writeHead(200, {'Content-Type' : 'text/plain'})
  res.end('this is two')
}).listen(port, host, () => console.log(`running at ${host}:${port}`))

