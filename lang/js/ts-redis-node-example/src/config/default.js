/* eslint-disable @typescript-eslint/no-var-requires */
const { version } = require('../../package.json')
const { resolve } = require('path')

module.exports = {
  redisUrl: 'xxx.com',
  version,
  server: resolve(__dirname, '..', 'server'),
  scheme: 'http',
  host: 'localhost',
  port: 4000,
}
