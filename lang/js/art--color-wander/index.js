const createConfig = require('./config')
const createFileRenderer = require('./lib/create-file-renderer')

const seed = typeof process.argv[2] !== 'undefined'
  ? String(process.argv[2])
  : undefined

const config = createConfig(seed)

createFileRenderer(config)
