var canvas = document.querySelector('#paint')
var ctx = canvas.getContext('2d')
var splat = document.querySelector('#splat')
var splatStyle = getComputedStyle(splat)
canvas.width = parseInt(splatStyle.getPropertyValue('width'))
canvas.height = parseInt(splatStyle.getPropertyValue('height'))

var mouse = {x: 0, y: 0}

canvas.addEventListener('mousemove', function (e) {
  mouse.x = e.pageX - this.offsetLeft
  mouse.y = e.pageY - this.offsetTop
}, false)

ctx.lineWidth = 2
ctx.lineJoin = 'round'
ctx.lineCap = 'round'
ctx.strokeStyle = 'rebeccapurple'

canvas.addEventListener('mousedown', function (e) {
  ctx.beginPath()
  ctx.moveTo(mouse.x, mouse.y)

  canvas.addEventListener('mousemove', onPaint, false)
}, false)

canvas.addEventListener('mouseup', function () {
  canvas.removeEventListener('mousemove', onPaint, false)
}, false)

var onPaint = function () {
  ctx.lineTo(mouse.x, mouse.y)
  ctx.stroke()
}

