// codecardio

// functions 26
function getPerson) {
  var person = new Object {
    this.name = "lashawnda",
    this.age = "32",
  }
  return(person[name, age]);
}

function getPerson) {
  var person = new Object {
    this.name = "lashawnda",
    this.age = "32",
  }
  return(person.name, person.age);
}

// functions 28
var car = {
  make: "subie",
  model: "justy" };

function newCar(){
  var car = new car(make:"Subie", model:"Justy") ;
  return(car.make, car.model);
}


function newCar(){
  var car = { make: "subie",
            model: "justy" };
  return(car.make, car.model);
}

// functions 29
var person = person {
  name:"lashawnda",
  age:"32", };

function canDrive() {
  if
    (person.age >= 16)
    return(person.name + " is old enough to drive.")
  else if
    (person.age < 16)
    return(person.name + " is not old enough to drive.")
  else
    return(person.name + " must not be human, I guess.")
}

// functions 30-?
var user = {role:"bro", email:"bro@bro.bro" }


// functions 31
var user = { email:"bro@bro.bro" };

function addRole(x) {
  var x = user.role;
  alert(x);
}

// functions 32
var user = {role:"bro", email:"bro@bro.bro" };

function removeRole(user);
  delete user.role;
  return(user);
}

// functions 33
var person = { name:"lashawnda", age:"32", gender:"transwoman", career:"cat watcher" };
function unknownPerson() {
  for (var value in person) {
    if (person.hasOwnProperty(value));
    value = "Unknown";
  }
}
