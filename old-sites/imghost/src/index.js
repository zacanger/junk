const { existsSync, statSync } = require('fs')
const { extname, resolve } = require('path')
const glob = require('glob')
const _ = require('lodash')
const express = require('express')
const bb = require('express-busboy')
const sanitizeFilename = require('sanitize-filename')
const Paginator = require('paginator')
const pkg = require('../package.json')
const { getSlug, handleError, listenLog, moveFile } = require('./util')
const { homePage, listPage } = require('./pages')
const { imagePath } = require('./config')
const paginator = new Paginator(48, 8)
const app = express()
const statCache = {}
const pub = resolve(__dirname, '..', 'public')

app.use(express.static(pub))
app.use(express.static(imagePath))
bb.extend(app, { upload: true })

app.get('/', (req, res) => {
  res.send(homePage())
})

app.get('/diag', (req, res) => {
  res.json({ name: pkg.name, version: pkg.version })
})

app.post('/upload', (req, res) => {
  if (!req.files || !req.files.file) {
    return handleError(res, 'No file specified.')
  }

  const file = req.files.file
  const ext = req.query.ext
    ? sanitizeFilename(req.query.ext)
    : extname(file.filename)
  const getName = (n) => `${imagePath}/${n}${ext}`

  if (ext.toLowerCase() === '.php') {
    return handleError(res, 'lol go away')
  }

  let name = getSlug(file.file)
  const fn = file.filename.replace(ext, '')

  if (existsSync(getName(name))) {
    name = `${fn}-${name}`
  }

  if (existsSync(getName(name))) {
    name = `${name.replace(`${fn}-`, '')}-${fn}`
  }

  if (existsSync(getName(name))) {
    return handleError(
      res,
      'Could not generate semi-unique filename based on hash of file contents and filename.'
    )
  }

  moveFile(file.file, getName(name), (err) => {
    if (err) {
      console.trace(err)
      return
    }

    res.redirect(`/${name}${ext}`)
  })
})

app.get('/list/:page?', (req, res) => {
  glob('*.*', { cwd: imagePath }, (err, files) => {
    if (err) {
      throw err
    }
    let page =
      typeof req.params.page !== 'undefined' ? parseInt(req.params.page, 10) : 0
    page = Math.min(Math.max(0, page), files.length)

    const paginationInfo = paginator.build(files.length, page)

    const fullFiles = _.reverse(
      _.sortBy(
        _.map(files, (f) => {
          if (statCache[f]) {
            return statCache[f]
          }
          const stat = statSync(`${imagePath}/${f}`)
          const o = {
            name: f,
            size: stat.size,
            mtime: stat.mtime,
          }
          statCache[f] = o
          return o
        }),
        'mtime'
      )
    )

    res.send(
      listPage({
        paginationInfo,
        pages: _.range(paginationInfo.first_page, paginationInfo.last_page),
        files: _.slice(
          fullFiles,
          paginationInfo.first_result,
          paginationInfo.last_result + 1
        ),
      })
    )
  })
})

listenLog(app)
