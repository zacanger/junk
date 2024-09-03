// Once you complete a problem, open up Chrome and check the answer in the console.

var name = 'Tyler'

// Create a function called isTyler that accepts name as it's only argument.
// If the argument you passed in is equal to 'Tyler', return true. If it's not, return false.

function isTyler (name) {
  if (name === 'Tyler') {
    return true
  } else {
    return false
  }
}

// Create a function called getName that uses prompt() to prompt the user for their name, then returns the name.

function getName () {
  var name = prompt("What's your name?")
  return ('Howdy, ' + name + '!')
}

// Create a function called welcome that uses your getName function you created in the previous problem to get the users name,
// then alerts "Welcome, " plus whatever the users name is.

// oh... uh, see my answer to the previous problem?

// What is the difference between arguments and parameters?

// parameters: could be thought of sort of as the placeholders or variables that get passed to something
// arguments: the 'real' values that you would actually pass.

// What are all the falsy values in JavaScript and how do you check if something is falsy?

// Nan, null, false, undefined, 0, "".
console.log(!!(null))
// or
function isThisFalse (arg) {
  if (arg === false) {
    return false; } else {
    return ('Nope, not false, sorry.'); }
}

// Create a function called myName that returns your name

function myName () {
  var z = 'Zac Anger'
  return z
}

// Now save the function definition of myName into a new variable called newMyName

var newMyName = myName()

// Now alert the result of invoking newMyName

alert(newMyName)

// Create a function called outerFn which returns an anonymous function which returns your name.

function outerFn () {
  return function () {
    return newMyName
  }
}

// Now save the result of invoking outerFn into a variable called innerFn.

var innerFn = outerFn()

// Now invoke innerFn.

console.log(innerFn())

