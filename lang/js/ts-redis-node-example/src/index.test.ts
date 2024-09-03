/* eslint-env jest */

import http from 'http'
import { app } from '.'
import { redisInstance } from './db'
// eslint-disable-next-line node/no-unpublished-import
import request from 'supertest'

jest.setTimeout(30_000)

describe('items', () => {
  let server = null

  beforeEach(async () => {
    server = http.createServer(app.callback())
  })

  afterEach(() => {
    server.close()
  })

  afterAll(() => {
    redisInstance.quit()
  })

  test('GET /items', async () => {
    const res = await request(server).get('/items')
    expect(res.status).toEqual(200)
    expect(Array.isArray(res.body)).toBe(true)
  })

  test('GET /items/foo', async () => {
    const res = await request(server).get('/items/foo')
    expect(res.status).toEqual(200)
    expect(Array.isArray(res.body)).toBe(true)
  })
})
