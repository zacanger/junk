// fits the PIXI.Sprite to the parent
// similar to CSS background-size: cover

module.exports = (ctx, image, parent, scale) => {
  scale = typeof scale === 'number' ? scale : 1
  parent = typeof parent === 'undefined' ? ctx.canvas : parent

  let tAspect = image.width / image.height
  let pWidth = parent.width
  let pHeight = parent.height
  let pAspect = pWidth / pHeight
  let width
  let height
  if (tAspect > pAspect) {
    height = pHeight
    width = height * tAspect
  } else {
    width = pWidth
    height = width / tAspect
  }
  width *= scale
  height *= scale
  let x = (pWidth - width) / 2
  let y = (pHeight - height) / 2
  ctx.drawImage(image, x, y, width, height)
}
