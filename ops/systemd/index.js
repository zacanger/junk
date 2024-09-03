// let's put this file under /opt/testserver

const
  http = require('http')
, port = process.env.PORT || 3000
, host = process.env.HOST || '127.0.0.1'

http.createServer((req, res) => {
  res.writeHead(200, {'Content-Type' : 'text/plain'})
  res.end('sup, brah')
}).listen(port, host, () => console.log(`running at ${host}:${port}`))

