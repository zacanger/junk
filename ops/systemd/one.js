const
  http = require('http')
, port = process.env.PORT || 9999
, host = process.env.HOST || '127.0.0.1'

http.createServer((req, res) => {
  res.writeHead(200, {'Content-Type' : 'text/plain'})
  res.end('this is one')
}).listen(port, host, () => console.log(`running at ${host}:${port}`))

