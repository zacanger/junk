module.exports = (randFunc) =>
  (min, max) => {
    if (typeof min === 'undefined') {
      min = 1
    }
    if (typeof max === 'undefined') {
      max = min
      min = 0
    }
    return randFunc() * (max - min) + min
  }
