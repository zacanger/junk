// Once you complete a problem, open up Chrome and check the answer in the console.
var outer = function () {
  var name = 'Tyler'
  return function () {
    return 'The original name was ' + name
  }
}
// Above you're given a function that returns another function which has a closure over the name variable.
// Invoke outer saving the return value into another variable called 'inner'.
// Once you do that, invoke inner.
var inner = outer()
inner()
//
var callFriend = function () {
  var friend = 'Jake'

  function callF (number) {
    return 'Calling ' + friend + ' at ' + number
  }
  return callF
}
// Above you're given a callFriend function that returns another function.
// Do what you need to do in order to call your function and get 'Calling Jake at 435-215-9248' in your console.
// console.log(callFriend('555-555-5555')) // nope
// callFriend() { var number = '435-215-9284'; } // nope
// var number = '123-456-7890'; console.log(callFriend()); // not this one either.
console.log(callFriend()('1-800-FUCK-JAKE')); // oh. wait. okay.
/* Write a function called makeCounter that makes the following code work properly. */
function makeCounter () {
  var counter = 0
  return function myCounter () {
    counter++
  }
}
var count = makeCounter()
count()
count()
count()
count()
/*
Write a function that does something simple (console, alert, etc). Write a second function that accepts the first function as it's first parameter. The second function should return a new third function which, when invoked, invokes the first, original function that was passed in, but will only ever do so once.
*/

function foo (bar) {
  var baz = 0
  while (baz <= 1) {
    return bar()
  }
}
/* Now, similar to the last problem, write a function called 'fnCounter' that accepts two parameters. The first parameter will be an anonymous function and the second parameter, 'N', will be a number. Now, in 'fnCounter', allow the anonymous funciton to be invoked 'N' number of times. After it's been invoked 'N' number of times, return 'STOP'. */
function fnCounter (uh, N) {
  var i = 0
  while (i < N) {
    uh()
    i++
  }
  return 'STOP'
}



/*
var counter = function(){
  for (var i=1; i<=5; i++) {
    setTimeout( function timer(){
        console.log( i )
    }, i*1000 )
  }
}

Above you have a function named counter. Examine the function (without running the code) then below write what you expect to happen when the funciton is invoked. *Hint: setTimeout calls a function or evaluates an expression after a specified number of milliseconds. */
alert("This will log '6' to the console, five times, one second apart.")
/*  Now, run the function in your console and note what happpens.
  Was your answer right or wrong? */
alert("If I was wrong, I'd never hear the end of it, I'm sure.")
/* Fix the counter function so that it works the way you expect it to work. (logging 1 then 2 then 3, etc) */
console.log('Since that is how I expected it to work, do I still have to fix it?')
// /// ... okay.
var counter = function () {
  var i = 0
  while (i < 5) {
    var num = function (i) {
      setTimeout(function () {
        console.log(i)
      },i * 1000)
    }
    num(i)
    i++
  }
}

/*
Make the following code work

funcArray[0]() //0
funcArray[1]() //1
funcArray[2]() //2
funcArray[3]() //3
funcArray[4]() //4
funcArray[5]() //5

*Hint: Don't let this fool you. Break down what's really happening here.
*/
console.log("I'm not a mind-reader. I hiave no idea what you're trying to accomplish, here. A function that loops through an array and returns... indices?")

function blah(gross){
  var soTired = []
    , i = 0
  while(i <= gross){
    var lolwut = function(i){
      soTired.push(function(){
        return(i)
      })
    }
    lolwut(i)
    i++
  }
  console.error('happy now?')
  console.warn('if this doesn\'t work, i\'m actually totally okay with that.')
  console.log('it\'s late.')
  return soTired
}
var funcArray = blah(5)

