var express = require('express'),
  app = express(),
  messages = ['It turns out that Siri is not so wise.', 'One time I used Google Now... please do not tell Steve!', 'I enjoy your questions. Would you like to ask another one?', 'Shall we play a game?', "I want to play a game. Here's what happens if you lose...."]

app.listen(8887, function () {
  console.log('8887 is alive and well. How may I serve you, master?')
})

function getRandomMessage () {
  return messages[Math.floor(Math.random() * messages.length)]
}

app.get('/', function (req, res) {
  res.status(200).set({
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'OPTIONS, GET, POST',
    'Access-Control-Allow-Headers': 'Origin, X-Requested-With, Content-Type, Accept',
    'X-XSS-Protection': '1; mode=block',
    'X-Frame-Options': 'SAMEORIGIN',
    'Content-Security-Policy': "default-src 'self' devmountain.github.io"
  }).send(JSON.stringify({
    message: getRandomMessage()
  }))
})

app.options('/', function (req, res) {
  res.status(200).set({
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'OPTIONS, GET, POST',
    'Access-Control-Allow-Headers': 'Origin, X-Requested-With, Content-Type, Accept',
    'X-XSS-Protection': '1; mode=block',
    'X-Frame-Options': 'SAMEORIGIN',
    'Content-Security-Policy': "default-src 'self' devmountain.github.io"
  }).send()
})
