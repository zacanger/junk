// Variables

// JavaScript uses "lexical" scoping for variables, meaning it matches
// a variable based on its name.  "Name" isn't the same as "name".

// Variables hold values (Numbers, Strings, Arrays, Objects, Functions, Boolean)
var myAge = 39;
var myName = 'Jared';
var myKids = ['Isaac','Abbi','Natalia','Camila','Bella'];

// Conditional Statements (if/else)

// Conditional statements use comparison operators to produce a
// Boolean result (true/false)
if (myAge < 20) {
  // do something
} else if (myAge < 40) {

} else {
  // do something else
}

// Truthy and Falsy
// There are 6 values that are considered "falsy"
// '',0,false,undefined,null,NaN


// == vs ===
// The double equals will convert the string to a number
// The triple equals will return false if the values aren't
// the same type
if ('42' == 42) {
  console.log("'42' == 42");
}

if ('42' === 42) {
  console.log("'42' === 42");
}

// Function Declaration
function DoubleANumber(number) {
  console.log(number * 2);
}

// A function can also return a value
function DoubleANumber2(number) {
  return number * 2;
}

// Function Invocation

DoubleANumber(10);

// If a function returns a value, we save that in a variable
var doubled = DoubleANumber2(10);
console.log(doubled);

// Function Expression

// Functions themselves can be stored in a variable
var func = function SayHello() {
  console.log("Hello");
};

// When a function is used in an expression, the function name
// isn't required.  This is called an anonymous function
var func1 = function () {
  console.log("Hello");
};

// We'll see where using anonymous functions is a common
// pattern in a future lesson

// Scope
var name = "Jared";

// A function can access variables declared outside the function
function SayMyName() {
  console.log(name);
}

SayMyName();

// A function argument is "local" to the function, and "hides"
// the outer variable with the same name
function SayMyName2(name) {
  console.log(name);
}

SayMyName2("John");

// A variabled declared inside the function also "hides" the
// outer variable with the same name
function SayMyName3() {
  var name = "Bob";
  console.log(name);
}

SayMyName3();

// But variables declared inside a function are only
// visible inside the function
function ShowAge() {
  var age = 10;
  console.log(age);
}

//console.log(age) // This is an error because "age" isn't defined

// Functions can be "nested" inside of other functions.  Scope works
// from the inside-out.
function Grandpa() {
  var name = "Grandpa";
  function Dad() {
    var name = "Dad";
    function Son() {
      var name = "Son";
    }
  }
}

// There are two rules to remember with functions
// 1. A function can access the the variables declared in outer functions
// 2. A function can access those variables even after the outer function
//    has been invoked and returned.
// A function that has "remembered" these values is called a closure.

// Closures
function Person(firstName, lastName) {
  function SayMyName() {
    console.log(firstName + ' ' + lastName);
  }
  console.log('All done');
  return SayMyName;
}

var person = Person('Jared','Stark');
person();

.call(scope, arg1, arg2, etc) // takes ARGUMENTS, comma sep
.apply(scope, [arg1, arg2, etc])// takes ARRAY of arguments
using .call() or .apply() invokes the function, so no need to invoke like fun();
.bind() does NOT invoke, just binds values prior to invocation.
// this way one can bind values WITHOUT calling it, so no need to create another inner function
// and go through lots of work to avoid screwing up scope

