var express = require('express')
	, app = express()
	, bodyparser = require('body-parser')
	, cors = require('cors')
	, mongo = require('mongojs')
	, port= 9999
	, db = mongo('menu')
	, Chocolate = db.collection('chocolate')

app.use(bodyparser.json())
app.use(cors())

var belgian = {
	price: 18,
	size: '5 lb',
	calories: 8000
}

app.post('/api/food', function(req, res){
	Chocolate.insert(req.body, function(err, result){
		if (err) {
			res.status(500).send(err)
		} else {
			res.send(result)
		}
	})
})

app.get('/api/food', function(req, res){
	Chocolate.find({}), function(req, res){
		if (Err) {
			res.status(500).send(err)
		} else {
			res.send(data)
		}
	}
})

app.delete('/api/delete:id', mongojs.ObjectId(req.params.it), function(req, res)){
	Chocolate.remove({'id': req.params.id}), function(err, result){
		if (err){
			res.status(500).send(Err)

		} else {
			res.send(result)
		}
	})
})

// VIM WHAT THE FUCK ARE YOU DOING
// I GIVE UP ON THIS ONE
// VIM FOUGHT ME EVERY STEP OF THE WAY
// app.put('/api/food/:id', function(req, res){
// 	food.update({'_id': mongojs.ObjectId(req.params.id), function(err, data)
// 							if (err){
// 								res.status(500).send(err)
//
// 							} else {
// 								res.send(data)
// 							}
// 	})
// })


app.listen(port, function{
	console.log('oi oi oi, ' + port + ' is ready, yo!')
})

