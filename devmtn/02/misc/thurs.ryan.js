function checkEvens( num ) {
  if (num % 2 === 0) {    // this just checks if the argument passed is divisible by two
    return true;
  } 
}

function compareLengths( originalArr, newArr ) {
  if (originalArr > newArr) {
    return 'More odds than evens.';   // this is pretty self-explanitory;
  } else if (originalArr < newArr) {  // just comparing lengths (whodathunkit)
    return 'More evens than odds.';   // and returning based on which is longer
  } else {
    return 'Same number of evens and odds';
  }
}

function howMany( originalArr, newArr, evensOrOdds ) {
  if (evensOrOdds === 'More odds than evens.') {
    var length = originalArr.length - newArr.length; // this is the bit where things are
    console.log('There are ' + length + ' more odds than evens.');
  } else if (evensOrOdds === 'More evens than odds.') {
    var length = newArr.length - (originalArr.length - newArr.length);
    console.log('There are ' + length + ' more evens than odds.');
  } else {
    console.log('There are the same number of evens and odds.');
  }
} 

function myCallbacks( iterator, lengthCompare, counter, arr ) {
  var newArray = [];                          // hey hey hey, a sweet new array
  arr.forEach(function( num, index ) {        // let's loop over the old array first
    if (checkEvens(num)) {                    // see if them items is %2===0
      newArray.push(checkEvens(num));         // if they is... i can't keep that up...
    }                                         // we just push them to the new array.
  });
  var evensOrOdds = lengthCompare(arr, newArray); // we compare lengths of the old array and new array
  counter(arr, newArray, evensOrOdds);            // we pass the old array, the new array, and the variable above
}                                                 // to the parameter from the top of this function

var myArray = [1, 2, ,3 ,4 ,5 ,6, 7, 8, 9, 10, 11]; // oh, HERE's that array we keep talking about

myCallbacks(checkEvens, compareLengths, howMany, myArray); // let's invoke that function and pass it ALL THE THINGS

