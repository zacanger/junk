import { resolve } from 'path'
process.env.NODE_CONFIG_DIR = resolve(__dirname, 'config')

const isTest = process.env.NODE_ENV === 'test'

import * as http from 'http'
import config from 'config'
import Koa from 'koa'
import Router from '@koa/router'
import * as middleware from 'koa-mid'
import Db, { redisInstance } from './db'

const port: number = config.get('port')
export const app: Koa = new Koa()
const router = new Router()
const db = new Db()

// @ts-ignore
app.port = port

router.post('/items', async (ctx) => {
  // @ts-ignore
  const body = ctx.request.body
  const { name, foo, bar } = body
  await db.update({ name, foo, bar })
  ctx.body = null
})

router.get('/items', async (ctx) => {
  ctx.type = 'application/json'
  ctx.body = await db.get('*')
})

router.get('/items/:item', async (ctx) => {
  ctx.type = 'application/json'
  ctx.body = await db.get(ctx.params.item)
})

middleware(app)
app.use(router.routes())

const handler = app.callback()

const server = http.createServer((req, res) => {
  handler(req, res)
})

const main = () => {
  // @ts-ignore
  server.listen(app.port, () => {
    // @ts-ignore
    console.log('listening on', app.port)
  })

  process.on('SIGTERM', () => {
    server.close(() => {
      redisInstance.quit(() => {
        process.exit(0)
      })
    })
  })
}

if (!isTest) {
  main()
}
