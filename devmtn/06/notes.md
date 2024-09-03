#### req.params vs req.query
Params and Query do the same thing.
Params is hardcoded into endpoint; query allows some flexibility.
Query uses http://url.url/api/endpoint?whatever=something
Param would be more like http://url.url/api/endpoint/something
```javascript
app.get('api/hobbies/:type/:fun', function( req, res ) {
    req.params.type // boring
    hobbies.forEach(function( hobby) {
        if (hobby.type = req.params.type) {
            res.send(hobby);
        }
    })
});

localhost:8989/api/hobbies/boring/no

app.get('api/hobbies', function( req, res ) {
    req.query.type // boring
    hobbies.forEach(function( hobby) {
        if (hobby.type = req.query.type) {
            res.send(hobby);
        }
    })
});

localhost:8989/api/hobbies?type=boring&fun=no


var hobbies = [];
```

#### how to not need to use express-cors (or whatever-cors)
```javascript
var express = require('express');
var request = require('request');

var app = express();
app.use('/', function(req, res) {
  var url = apiServerHost + req.url;
  req.pipe(request(url)).pipe(res);
});

app.listen(process.env.PORT || 3000);
```
OBVIOUSLY this only applies when everything's local; this is a proxy server, essentially, sort of.

also there's an express builtin: `app.use(express.static()`, eg `app.use(express.static('public'))`


#### monogooooooooooo(se)
Turns out if you send Mongoose anything that's not in your schema, it's just like "Oh, okay, I'm not interested" and tosses that bit out, I guess.

When we're not doing eg `require('./path')` (but rather `require(/path)' with no dot first`), it looks in `node_modules/`. Obvs usually we'll probably look from cwd instead. Any way to look from project root by default?
``


getStuff: function(req, res){
  Stuff.find().populate('things.whatevers').exec()
    .then(function(data, err)
// etc., etc., etc., etc.
