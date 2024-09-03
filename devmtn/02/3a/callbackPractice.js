/* In this repo your job is to write functions to make each function call work properly.
Below is a sample problem

  //code here for sayHi

   sayHi('Hi Katie', function(thingToSay){
      alert(thingToSay);
   });

and what you should write is the sayHi function that makes the code above work,

   var sayHi = function(str, cb){
    cb(str);
   }

   sayHi('Hi Katie', function(thingToSay){
      alert(thingToSay); //should alert ('Hi Katie')'
   });
*/

/* FIRST PROBLEM ---- FIRST PROBLEM --- FIRST PROBLEM --- FIRST PROBLEM */

function first () {
  return names[0];
}

var names = ['Tyler', 'Cahlan', 'Ryan', 'Colt', 'Tyler', 'Blaine', 'Cahlan'];
first(names, function (firstName) {
  console.log('The first name in names is ' + firstName);
});

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

function last () {
  return names[names.length - 1];
}

var names = ['Tyler', 'Cahlan', 'Ryan', 'Colt', 'Tyler', 'Blaine', 'Cahlan'];
last(names, function (lastName) {
  console.log('The last name in names is ' + lastName);
});

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

function multiply (i1, i2) {
  return (i1 * i2);
}

multiply(4, 3, function (answer) {
  console.log('The answer is ' + answer); // should console.log 12
});

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

var names = ['Tyler', 'Cahlan', 'Ryan', 'Colt', 'Tyler', 'Blaine', 'Cahlan'];
contains(names, 'Colt', function (result) {
  if (result === true) {
    console.log('Colt is in the array');
  } else {
    console.log('Colt is not in the array');
  }
});

function contains (x, names) {
  if (names.indexOf(x))
}

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

var names = ['Tyler', 'Cahlan', 'Ryan', 'Colt', 'Tyler', 'Blaine', 'Cahlan'];

uniq(names, function (uniqArr) {
  console.log('The new names array with all the duplicate items removed is ', uniqArr);
});

function uniq (arr, callback) {
  var uniqArr = [];
  arr.forEach(function (name) {
    if (uniqArr.indexOf(name) == -1) {
      uniqArr.push(name);
    }
  });
}

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

var names = ['Tyler', 'Cahlan', 'Ryan', 'Colt', 'Tyler', 'Blaine', 'Cahlan'];
// i changed the word 'indice' here to 'index', because 'indice' hasn't been in
// common usage in a couple hundred years...
each(names, function (item, index) {
  console.log('The item in the ' + index + ' position is ' + item);
});

function each (names, callback) {
  names.forEach(function (item) {
    callback(item, index);
    index++;
  });
}

/* NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM ---- NEXT PROBLEM */

var users = [{
  id: '12d',
  email: 'tyler@gmail.com',
  name: 'Tyler',
  address: '167 East 500 North'
}, {
  id: '15a',
  email: 'cahlan@gmail.com',
  name: 'Cahlan',
  address: '135 East 320 North'
}, {
  id: '16t',
  email: 'ryan@gmail.com',
  name: 'Ryan',
  address: '192 East 32 North'
}, ];

function getUserById (users, name, callback) {
  users.forEach(function (item) {
    if (item.id === name) {
      callback(item);
    }
  });
}

var id = '12d';

getUserById(users, id, function (user) { // i changed this to accept an argument for the id, and the line below to return that, rather than just '16t'
  console.log('The user with the id ' + id + ' has the email of ' + user.email + ' the name of ' + user.name +
    ' and the address of ' + user.address);
}); // otherwise it can only ever return ryan's info. poor cahlan and tyler. :(

