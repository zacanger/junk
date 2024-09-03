// We're in a job interview. Answer the following questions (try to not look at your notes unless you have to).

// 1) What is the purpose of the 'this keyword'?
// The `this` keyword is a shorthand way of directly accessing its parent's
// properties. For example, one could have an object describing a customer,
// wherein one key might be for a phone number; let's call that `tel`,  and
// its value is `867-5309` ...
// So, if we had a function inside that object -- that is, a method --
// we could access that by using `this.email`, which would give us the string
// `867-5309`.

// 2) What are the four rules that govern what the 'this keyword' is bound to and describe each?
// (Note: there are four RULES, and also four BINDINGS; these are different things, evidently, but
// DevMountain uses the word RULES in place of BINDINGS, so these are just sort of the binding
// definitions, as I understand them.

// These rules could be summed up in one sentence:
// 'What is calling the object?'
// In more detail:

// IMPLICIT is the value attached to a keyword when it refers to an object
// and it applies to the PARENT object
//
// EXPLICITly using `this` (with call, apply, bind) allows one to use `this` in a different context.
//
// DEFAULT (window object), and
//
// NEW in creation of new objects.

// 3) What is the difference between call and apply?

// Call and apply are both functions that can only be called on other functions.
// However, call accepts both a first argument and *any additional arguments
// that are passed to it*. Apply takes an *array* instead of additional
// arguments, which it 'unpacks' as arguments for the function.

// 4) What does .bind do?

// Unlike call and apply, bind doesn't immediately invoke that function.
// In a way, it's a shortcut to make a new function for you

// Create an object called user which has the following properties.
// username --> which is a string
// email --> which is a string
// getUsername --> which is a function that returns the current object's username property. *Don't use 'user' instead use the 'this' keyword*

var user = {
  username: 'zee',
  email: 'zee@zee.zee',
  getUsername: function () {
    return this.username
  }
}

// Now, invoke the getUsername method and verify you got the username of the object and not anything else.

user.getUsername()

// Write the function definitions which will make the following function invocations function properly.

var Car = function (make, model, year) {
  this.make = make
  this.model = model
  this.year = year
  this.moveCar = function () {
    this.move = 0
    this.move += 10
    return this.move
  }
}

var Prius = new Car('Toyota', 'Prius', 2011)
var Mustang = new Car('Ford', 'Mustang', 2013)

Prius.moveCar(); // increments Prius's move property by 10. Returns the new move property.
Mustang.moveCar(); // increments Mustang's move property by 10. Returns the new move property.

// You'll need to write a moveCar function which is added to every object, and is being
// returned from the Car function. You'll also need to use the 'this' keyword properly
// in order to make sure you're invoking moveCar on the correct object.

var getYear = function () {
  return this.year
}

/* Above you're given the getYear function. Using your Prius and Mustang objects from above, use the proper syntax that will allow for you to call the getYear function with the Prius, then the Mustang objects being the focal objects. **Don't add getYear as a property on both objects**. */

getYear.call(Prius)
getYear.call(Mustang)

//

var user = {
  username: 'iliketurtles',
  age: 13,
  email: 'iliketurtles@gmail.com'
}

var getUsername = function () {
  console.log(user.username)
}

setTimeout(getUsername, 5000)

// Above you're given an object, a function, and a setTimeout invocation. After 5 seconds, what will the getUsername function return?

// undefined

// In the example above, what is the 'this keyword' bound to when getUsername runs?

// its parent, so no property at all.

// Fix getUsername so that the user object will be the focal point when it is run.

/* Below answers were incorrect. Note that I've corrected the above comments to reflect that **actual** problem; will likely submit PRs at some point, since almost all these exercies are written like this.... The original instructions are immediately below. */

// Fix the setTimeout invocation so that the user object will be the focal object when getUsername is ran.

// this is not correct...
// setTimeout(function() {
//   console.log(this.username)
// }, 5000)

// setTimeout(user.getUsername, 5000)
