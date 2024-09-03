Sample app using Redis as a data store

## Usage

```
GET / returns a simple view
GET /items returns all items
GET /items/:itemname returns :itemname info
POST /items updates an item info

type info = {
  name: string // required
  foo: string // required
  bar: string // required
}
```
