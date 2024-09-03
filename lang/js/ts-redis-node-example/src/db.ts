import redis from 'async-redis'
import config from 'config'

const redisUrl: string = config.get('redisUrl')

export let redisInstance = null

export default class Db {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  client: any

  constructor() {
    if (!(this instanceof Db)) {
      return new Db()
    }

    const client = (this.client = redisInstance = redis.createClient(
      6379,
      redisUrl
    ))

    client.on('error', (err) => {
      console.error(err)
    })
  }

  async update({ name, foo, bar }) {
    await this.client.hset(`${name}:${foo}`, 'bar', bar)
  }

  async get(item: string = '*') {
    const keys = await this.client.keys(`${item}:*`)

    const foo = [...keys]
      .sort((a, b) => a.localeCompare(b))
      .map(async (key) => {
        const values = await this.client.hgetall(key)
        const [item, foo] = key.split(':')
        return {
          item,
          foo,
          ...values,
        }
      })

    return Promise.all(foo)
  }
}
