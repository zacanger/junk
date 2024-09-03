# Mongo | Day 2 | mongoose

## Overview
+ Day 1 -- CRUD w/ MongoJS
+ Day 2 -- Modeling w/ Mongoose
+ Day 3 -- Data Structures && Strategy

## Review
+ Quiz
+ Landscape
+ CRUD
  + general groupings for action definitions
  + comparison to MongoJS methods
+ Creating/Using A Controller
+ Documents DB Attributes
  + flexible properties (vs column creation)
  + loosely typed values (vs column data_type validation)

## Mongoose vs MongoJS Theory
+ Why/How Mongoose Adds:
  + Validations
  + Data_Types
  + Prototypical Functions
  + Code Management (chaining callbacks/promises)
+ Quiz

## Mongoose Lifecycle
+ Briefly Outline Use Case Using Prototype Discussion Above
  + Model
    1. Schema Defined -- Configuration For A Document Constructor
    2. Model Created -- Document Constructor
  + Request/Response
    1. Document||Query Instance Building
    2. Document||Query Pre Middleware Runs (explained later)
    3. Document||Query Request Sent To Mongo
    4. Document||Query Response Returned To Mongo
    5. Document||Query Post Middleware Runs (explained later)
    6. Callback/Promise invoked
+ Quiz

## Schemas
+ define the 'shape' of documents within
+ a schema is mapped to a collection
  + Data_Type
  + Validations
    + Required
  + Indexing
  + Unique
+ Quiz

## Models
+ fancy constructors compiled from schema definitions
+ instances of models represent documents

## CRUD w/ Mongoose
+ Documents
  + Build
    + Create a new instance of a Model by passing in a JS Object
  + Execution Methods
    + save
+ Queries
  + Run Methods
    + nearly every method except save
  + Building Methods
    + where
    + limit
    + sort
    + select
  + Execution Method
    + exec -- sends request && returns promise

### Middleware && Hooks
+ Inject functions to be run at certain points in the lifecycle for certain actions
+ Lifecycle Hooks
  + pre
  + post
+ Methods
  + save
  + remove
  + count
  + find
  + findOne
  + findOneAndRemove
  + findOneAndUpdate
  + update

## WARNING
+ Documents returned from Mongo are NOT plain objects!!!
  + Document\#toObject must be called for expected behavior

## Summary
+ I Mongo on vacation. Take luck.

## Credit
Some text taken from http://mongoosejs.com




Students will know the difference between mongoose and mongo

Students will know how to make a mongoose model

Students will know how to define schema for a model

Students will know how to define types on mongoose schema properties

Students will know how to do CRUD operations using a mongoose model

Students will understand mongoose middleware

There exist "hook"s that we can watch on our models when data is saved. There are "pre" and "post" middleware that we can watch.
For example, we could make sure a user's first name is capitalized before it's saved into a database:
var schema = new mongoose.Schema({
        name: String,
        age: Number
});
schema.pre('save', function(next) {
        this.name = this.name.charAt(0).toUpperCase() + this.name.slice(1);
        next();
});
Students will understand database indexing

why use them and tradeoffs, specially space cost.

Students will understand mongoose .exec() , .where() , .limit() , and .sort()

Person
.find()
.where('role').equals('admin')
.where('age').gt(30)
.limit(10)
.sort(-'name)
.exec(function(err, persons) {
  //do this next
});
Students install and use bcrypt to hash a password and check against future log-ins

//basic schema
var schema = new mongoose.Schema({
        username: String,
        password: String
});
//middleware to save a hashed password
schema.pre('save', function(next) {
        var user = this;
        if (!user.isModified('password')) {
                return next();
        }
        bcrypt.genSalt(12, function(err, salt) {
                if (err) return next(err);
                bcrypt.hash(user.password, salt, function(err, hash) {
                        if (err) return next(err);
                        user.password = hash;
                        next();
                });
        });
});
//Login comparison method
schema.methods.comparePassword = function(pass) {
        var deferred = q.defer();
        bcrypt.compare(pass, this.password, function(err, isMatch) {
                if (err) {
                        return deferred.reject(err);
                }
                return deferred.resolve(isMatch);
        }
        return deferred.promise;
};
//Usage inside a controller
User.findOne({email: req.body.email}).exec().then(function(err, user) {
user.comparePassword(req.body.password)
        .then(function(isMatch) {
                if (isMatch) {
                        //log them in!
                }
        })
        .catch(function(err) {
                //no luck!
        });
});

# examples

```javascript
/// embedded
new_comment = {
  author: "Kyle",
  date: new Date(),
  text: "great book",
  votes: 5
};

db.posts.update(
  { text: "Destination Moon" },
  {
    '$push': { comments: new_comment },
    '$inc': { comments_count: 1 }
  }
);

db.posts.find({ 'comments.author': "Kyle" });

db.posts.find({ 'comments.votes': { $gt: 50 } });
```


-------------



# more examples


```javascript
////////////////////////////////////////////////////////////////////////////////
// Queries
////////////////////////////////////////////////////////////////////////////////

//find
Person
.find({
  occupation: 'engineer'
})
.exec(function(err, person) {
  //do this next
});

//findOne
Person
.findOne({
  email: 'cahlan@gmail.com'
})
.exec(function(err, person) {
  //do this next
});

//select
Person
.findOne({
  email: 'cahlan@gmail.com'
})
.select('_id email')
.exec(function(err, person) {
  //do this next
});
//--why would we use select? speed and efficiency--

//using where, limit, and sort
Person
.find()
.where('role').equals('admin')
.where('age').gt(30)
.limit(10)
.sort(-'name')
.exec(function(err, persons) {
  //do this next
});


////////////////////////////////////////////////////////////////////////////////
// Middleware
////////////////////////////////////////////////////////////////////////////////
var schema = new mongoose.Schema({
	name: String,
	age: Number
});
schema.pre('save', function(next) {
	this.name = this.name.charAt(0).toUpperCase() + this.name.slice(1);
	next();
});


// bcrypt

// Create a user schema with a username and password. Implement simple security on the user object by adding a hash and a salt using the bcrypt-nodejs.
var schema = new mongoose.Schema({
	username: String,
	password: String
});

schema.pre('save', function(next) {
	var user = this;
	if (!user.isModified('password')) {
		return next();
	}
	bcrypt.genSalt(12, function(err, salt) {
		if (err) return next(err);
		bcrypt.hash(user.password, salt, function(err, hash) {
			if (err) return next(err);
			user.password = hash;
			next();
		});
	});
});

// Now we have to compare passwords when a user is to log in, how would we do that?
schema.methods.comparePassword = function(pass) {
	var deferred = q.defer();
	bcrypt.compare(pass, this.password, function(err, isMatch) {
		if (err) {
			return deferred.reject(err);
		}
		return deferred.resolve(isMatch);
	});
	return deferred.promise;
};

//Usage inside a controller
User.findOne({email: req.body.email}).exec().then(function(err, user) {
user.comparePassword(req.body.password)
	.then(function(isMatch) {
		if (isMatch) {
			//log them in!
		}
	})
	.catch(function(err) {
		//no luck!
	});
});
```

-----------

-----------

# QUIZ TIME
------------------------
# Quiz | VS_Theory
1. Give a business case where Mongoose would be preferred to MongoJS
1. Give a business case where MongoJS would be preferred to Mongoose


# Quiz | Schemas
1. Write a userSchema that conforms to the following property requirements:
  + moniker
    + string
    + min length of 8
    + ensure this always exists
  + hair_style
    + string
    + only allow values: "on fleek", "busy bee", "SOS"
  + facial_hair
    + boolean
    + make this 'false' if not provided
  + number_of_eyes
    + number
    + max of 3

2. Given the schema below, sort the queries in order of most efficient to least efficient:
  ``` javascript
  {
  	name: String,
  	email: String,
  	bio: String,
  	createdAt: Date,
  	age: 11
  }
  ```

  ``` javascript
  User.find({name: function(doc) { return doc.charAt(0) === 'F'; }}).exec();
  User.findOne({name: function(doc) { return doc.charAt(0) === 'F'; }}).exec();
  User.findOne({name: new RegExp('^F'}).exec();
  User.findOne({bio: new RegExp('some phrase', 'g').exec();
  User.findOne({createdAt: {'$gt': new Date(2015, 02, 12)}}).exec();
  User.findOne({age: 12}).exec();
  ```

# Quiz | Review
1. Put the following Mongo components in a hierarchy
  + Documents
  + Databases
  + Collections
2. What data structure does Mongo use to store documents?
3. Name two ways document databases(noSQL) are different than table databases(SQL)
4. Are calls from Node to Mongo synchronous or asynchronous?


# Quiz | Lifecycle
1. What are the two many phases of the mongoose lifecycle?
2. What does having multiple phases in the req/res lifecycle help us accomplish?
3. In what ways is the req/res lifecycle for mongoose similar to req/res in Express?







