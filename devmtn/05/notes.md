if chain in express:
method, url, callback


app.all('/endpointOne', function(){
	console.log('a thing')
})

so it's one long chain of if statements. like we do manually. that's all.

express chain [{method: 'all', endpoint:'/endpoint', function(){}, {method: 'get', endpoint:'/nextone'}etc etc etc etc}]d

all endpoints are hit, with exception dependent on response

req and res are passed to EVERY function by express

ongoing request and response are passed on down whole if statement chain



a controller can't reference arrays from another file. so, your server (or, let's say, your index) couldn't have something defined that a ctrl needs to access (assuming your ctrl is set up as a an object, module.exports = {} with your functions(req, res, next) all in there.)

solution?

move your stuff into the file where you want to access it, i guess.

module.exports can be an array, too. that makes sense in a db context, okay.

