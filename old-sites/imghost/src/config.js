const { resolve } = require('path')
const p = resolve(__dirname, '..', 'imgs')

const legal =
  'You may upload any files that are legal under US law. Illegal or copyrighted material takedown requests can be sent to zac at zac anger dot com and will be respected, with a DMCA or court order. This is a free and open source software project available under the LGPL-3.0 License. Source code can be found at github.com/zacanger/imghost.'

module.exports = {
  imagePath: p,
  legal,
  name: 'imghost',
  port: 3000,
}
