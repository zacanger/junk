const fs = require('fs')
const path = require('path')
const seedRandom = require('seed-random')
const palettes = require('./lib/color-palettes.json')
const createRandomRange = require('./lib/random-range')
const maps = fs.readdirSync(path.resolve('maps')).map((f) => `maps/${f}`)

module.exports = (seed = String(Math.floor(Math.random() * 1000000))) => {
  const randomFunc = seedRandom(seed)
  const random = createRandomRange(randomFunc)
  const mapSrc = maps[ Math.floor(random(maps.length)) ]

  const arrayShuffle = (arr) => {
    let rand
    let tmp
    let len = arr.length
    let ret = arr.slice()
    while (len) {
      rand = Math.floor(random(1) * len--)
      tmp = ret[len]
      ret[len] = ret[rand]
      ret[rand] = tmp
    }
    return ret
  }

  const getPalette = () => {
    const paletteColors = palettes[Math.floor(random() * palettes.length)]
    return arrayShuffle(paletteColors)
  }

  console.log('Seed:', seed)

  return {
    // rendering options
    random: randomFunc,
    seedName: seed,
    pointilism: random(0, 0.1),
    noiseScalar: [ random(0.000001, 0.000001), random(0.0002, 0.004) ],
    globalAlpha: 0.5,
    startArea: random(0.0, 1.5),
    maxRadius: random(5, 100),
    lineStyle: random(1) > 0.5 ? 'round' : 'square',
    interval: random(0.001, 0.01),
    count: Math.floor(random(50, 2000)),
    steps: Math.floor(random(100, 1000)),
    endlessBrowser: false, // Whether to endlessly step in browser

    // background image that drives the algorithm
    debugLuma: false,
    backgroundScale: 1,
    backgorundFille: 'black',
    backgroundSrc: mapSrc,

    // browser/node options
    pixelRatio: 1,
    width: 1280 * 2,
    height: 720 * 2,
    palette: getPalette(),

    // node only options
    asVideoFrames: false,
    filename: 'render',
    outputDir: 'output'
  }
}
