var zacTheObject = {
  kathsDated         : ['kate', 'katherine', 'katie', 'kat', 'katrina', 'kath'],
  hair               : "reddish, sort of",
  eyes               : "really not very good",
  drinks             : "used to",
  likesToEatDogPoop  : false
}

console.log('Zac the Object has ' + zacTheObject.hair + ' hair, eyes that are ' + zacTheObject.eyes + ', and he drinks ' + zacTheObject.drinks + '... and does he like to eat dog poop?' + zacTheObject.likesToEatDogPoop)


for ( var i = 0; i <= zacTheObject.kathsDated.length; i++ ) {
  console.log (zacTheObject.kathsDated[i] + ' was one.')
}

for ( var key in zacTheObject ) {
console.log(zacTheObject[value])
}

for ( var key in zacTheObject ) {
  console.log(zacTheObject[key])
}


for ( var key in zacTheObject ) {
  if (zacTheObject.hasOwnProperty(key)) {
    delete zacTheObject[key]
  }
}
