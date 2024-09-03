#!/usr/bin/env node

const
  express = require('express')
, port    = 9000
, app     = express()

app
.use(express.static(__dirname + '/pub'))
.listen(port, () => console.log(`blog on ${port}`))

