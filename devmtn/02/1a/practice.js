// Create an object called me. Give it a key of name with the value being your name, and another key of age with the value being your age. Then alert your name using dot notation.
var me = {
  name: 'zacanger',
  age: 'not all that old!'
}
window.alert(me.name)
// Make a 'favoriteThings' object that contains the following keys: band, food, person, book, movie, holiday. Have the values to those keys be your favorite thing in that category.
var favouriteThings = {
  band: 'bt, but sometimes mewithoutYou... also manchester orchestra, and i really dig SNSD and f(x) and t-ara... um, this is a tough question.',
  food: 'chocolate, mostly, really. yeah.',
  person: 'i think it would be rude to choose a favourite.',
  book: 'oh gosh. probably Vanity Fair, by Thackeray... but there are so goddamn many.',
  movie: 'I HAVE AN ANSWER TO THIS ONE! the godfather, part ii. duh.',
  holiday: 'every holiday has serious downsides... can sunday be a holiday? i like sundays, usually.'
}
// After you've made your object, add another key named 'car' with the value being your favorite car and then another key named 'brand' with the value being your favorite brand.
favouriteThings.car = 'the kind that runs and gets okay gas mileage. maybe a subaru justy.'
favouriteThings.brand = 'really? um, the cheap kind. store brand. brand x.'
// Now change the food key in your favoriteThings object to be 'Lettuce' and change the book key in your favoriteThings object to be '50 Shades of Gray'.
favouriteThings.food = 'Lettuce... goddammit.'
favouriteThings.book = '50 Shades of Ugh You, asshole.'
/*Create an empty Object called backPack. Now, create a variable called 'item'
and set it equal to the string 'firstPocket'. Using bracket notation,
add a 'firstPocket' key (or property) to backPack, using 'item'.
Set the value of that key to 'chapstick'.
Using dot notation, add another key (or property) to your backPack object
that is named color, with the value being the color of your backpack. */
var backPack = { }
var item = 'firstPocket'
backPack[item] = 'chapstick'
backPack.color = 'bright ass pink. what about it?'
// After you do the above, alert your entire backPack object.
alert(backPack)
/*You probably noticed that it just alerted [object Object].
Alerting to see the data in your Object doesn't work so well.
Instead, console.log your whole backPack object and then check out the console. */
console.log(backPack)
// Create another 'me' object with the following properties name, age, height, gender, married, eyeColor, hairColor. Fill those properties in with the appropriate values.
var me = {
  name: 'z',
  age: '26',
  gender: 'i have one of those',
  married: 'really none of your business',
  eyeColor: 'green',
  hairColor: 'the rarest!'
}
// Now, loop through your object and alert every value. *Tyler --> 24 --> 6'0 --> Male, etc etc
for (var prop in me) {
  alert(prop)
}
// Create an Object called 'album' with 5 keys named different song titles that you make up, with the values being the length of each song.
var album = {
  aSubtleFluid: 8800,
  asAnAside: 230000,
  kiwi: 414000,
  whyIDont: 118000,
  josie: 480000
}
// Now, loop through your album object alerting every song title individually.
for (var key in album) {
  alert(key)
}
// Create an object called states that has 5 US states as properties with the values being their population (doesn't have to be accurate).
var states = {
  rhodeIsland: 1060000000,
  wyoming: 583153,
  utah: 1940000000,
  alaska: 736732,
  newHampshire: 1330000000
}
// Now, loop through your states object and if the states population is greater than 30K, alert that state.
for (var key in states) {
  alert(key)
  alert('All statues in the US have over 30,000 people. Try a valid question next time.')
}
//
var user = {
  name: 'Tyler McGinnis',
  email: null,
  pwHash: 'U+Ldlngx2BYQk',
  birthday: undefined,
  username: 'tylermcginnis33',
  age: 0
}
/*Above you're given a user object. Loop through the user object checking to make sure
that each value is truthy. If it's not truthy, remove it from the object. */
for (var prop in user) {
  if (!(user[prop])) {
    delete user[prop]
  }
}
// Once you get your truthy Object, Change the remaining values in the object to be specific to you (name: 'your name', username: 'your username'), rather than my information.
user.name = 'z'
user.username = 'zacanger'
user.pwHash = 'hahahahaha gtfo'
//
var user = {
  name: 'Tyler McGinnis',
  age: 24,
  pwHash: 'U+Ldlngx2BYQk',
  email: 'tylermcginnis33@gmail.com',
  birthday: '05/02/1990',
  username: 'tylermcginnis33',
  sayName: function () {
    alert('Email is : ' + this.email)
  }
}
// Let's say I, the user, decided to change my name and email address to the following
// name -> 'Tyler S. McGinnis', email -> 'tyler.mcginnis@devmounta.in'. Make that change.
user.name = 'Tyler S. McGinnis'
user.email = 'tyler.mcginnis@devmounta.in'
// Now call the sayName method that's on the user object which will alert the users email
user.sayName()
// Create an empty object called methodCollection.
var methodCollection = {}
/*Now add two methods (functions that are properties on objects) to your methodCollection
object. One called 'alertHello' which alerts 'hello' and another method called logHello
 which logs 'hello' to the console. */
methodCollection.alertHello = function () {
  alert('HelloQ')
}
methodCollection.logHello = function () {
  console.log('Hello again!')
}
// Now call your alertHello and logHello methods.
methodCollection.alertHello()
methodCollection.logHello()
// Create a function called MakePerson which takes in name, birthday, ssn as its parameters and returns a new object with all of the information that you passed in.
function MakePerson (name, birthday, ssn) {
  return {
    moniker: name,
    bornOn: birthday,
    superSecretImportantNumber: ssn
  }
}
// Create a function called MakeCard which takes in all the data it needs to make a Credit Card object and returns that object so that whenever you invoke MakeCard, you get a brand new credit card.
function MakeCard (name, mothersMaidenName, firstCatsBirthday, ssn, bankAccountNumber, childsFirstLove, burgersEatenInPastFiveYears, sexualActivity, valuationOnDeath, liquidity, fathersPromDate, emailPassword, voicemailPassword, insuranceStatements, anyOtherInvasiveInfoYouMightCareToShare) {
  return {
    number: 8675309867530900,
    expiry: 20160316,
    ccv: 555
  }
}
/* As of this point you should have a MakePerson and a MakeCard function which returns you either a person or a credit card object.
   Now, create a bindCard function that takes in a person object as its first parameter and a creditcard object as its second parameter.
   Have bindCard merge the two parameters together into a new object which contains all the properties from the person as well as the creditcard.
*/
var person = MakePerson()
var creditCard = MakeCard()

function bindCard (person, creditCard) {
  var financialInfo = {}
  for (var prop in person) {
    financialInfo[prop] = person[prop]
  }
  for (var prop in creditCard) {
    financialInfo[prop] = creditCard[prop]
  }
  return financialInfo
}
