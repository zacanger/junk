// make a loop. then have it go 1-5 endlessly. then have it only do it once.

// our way
// function count () {
//   while (true) {
//    for (i = 1; i <= 5; i++) {
//      console.log(i);
//     }
//   }
// }

// correct way
// function countOnce () {
//   for (var i = 0; i < 1; i++) {
function makeCounter () {
  var num = 0;
  return function () {
    if (num < 5) {
      return num++;
    } else {
      num = 0;
      return num;
    }
  };
}
//   }
// }

// other way

// function makeCounter() {
//	var x = 1;
//	retrun function() {
//		console.log(x1);
//		x++;
//		if (x>5) {
//			x=0;
//
//		}
//	}
// }

function makeCounter () {
  var counter = 1;
  var counter2 = 1;
  return function myCounter () {
    if (counter2 > 11) {
      return null;
    } else if (counter === 6) {
      counter = 0;
    }
    counter2++;
    return counter++;
  };
}

// Objects: less primitive than primitives.
// create an object literal that models a facebook message:

// var fbMsg = {
//  	sender: 'Jack';
//  	recipient: 'Jill';
//  	topic: ['water', 'pail', 'hill', 'cranial trauma']
//  	}

// or, using new Object();

var fbMsg = { };
fbMsg.sender = 'Jack';
fbMsg.recipient = 'Jill';
fbMsg.topic = ['water', 'pail', 'hill', 'cranial trauma'];

// removing from object, much less work than in array

delete message.recipient;

// brackets also work for accessing properties on an object

fbMsg['fuzzy date'] = 'yesterday afternoon';

// for loops over keys/values in an object:

for (var key in fbMsg) {
  console.log(key);
}

for (key in fbMsg) {
  console.log(fbMsg[value]);
}

// you don't really NEED to declare that something's a variable...
// that's bad practice, though. that'll make your whatever a global variable.
// 
// there are no constants in javascrpt, apparently.
// so, js isn't really modular. and scripts included in html will basically
// just override in loading order. so, a script tag by the footer would
// override one that's all the way up at the beginning of your body, if they
// had the same globals, for example.

// wrapping an entire function (even anonymous) in parens is a sweet idea!
// this way you're not polluting global namespace, and you can just
// invoke the 'function expression' immediately, so it's not really any
// different in practice than as if you'd left off the parents. so...

(function () {
  var yo = 'billybob';
  console.log('what up ' + yo);
})();(function () {
  var yo = 'margie-jo';
  console.log('what up ' + yo);
})();

// i guess a lot of minifiers will automatically just wrap all your scripts
// in these kind of expressions?

// a function is an object. it has a property called prototype.
// the keys of prototype (which is an object itself) are inhereted by any other
// object below that one.
//
// one can add to a prototype after declaring that function.
//
// a prototype describes the idea of a thing; here's all the things that would 
// be in a facebook message, y'know? so this way, we just made a CONSTRUCTOR. 
// and there's your use for the new Object syntax! now we know how to use that.
//

function Message(sender, recipient, topic) {
	this.from = sender;
	this.to = recipient;
	this.subject = topic;
}

// if you DON'T explicity definte a prototype, you'd get one anyway,
// it would basically just be implied/automatic.
// readability is a good reason to explicitly define it, though.
// also, you can set values in your prototype, obvs, that'll then be applied
// to all the stuffs that uses that prototype.

// // so we could also go ahead and comment out all that like so

function Message() {
}

Message.prototype = {
	to: ' ',
	from: ' ',
	about: ' ',
	date ' ' // note no comma on this line
}; 			 // also note that this could return stuff in ANY GODDAMN ORDER.

var msg = new Message ('joe', 'ruth', 'pickles and cream cheese');
var mssg - new Message ('ann', 'barb', 'i hear joe eats some weird stuff');
console.log(msg, mssg);

// this will make each one basically have the same structure (oh! prototype)
// is basically a framework for building objects!), but each INSTANCE is a 
// totally different actual object. okay.

// the keyword this: this.thing, etc... this just references the new (current 
// parent) object, that's all!i

// okay. a method is a property containing a function definition. that sounds
// more complicated than it needs to. a method is kind of like a little 
// function that works just on an object, i guess.
// or, in other words, it's literally just a property, the value of which 
// happens to be a function.

// okay, and so as was just cleared up, things that use the notation that 
// makes me think they're just being confusing and crap? yeah. basically just 
// properties of the string object, or the window object, or the browser 
// object, or the global (in node) object.

// but wait. what about node, really, seriously? i mean, is global the same 
// as browser? or window? because those are actual structures that would need 
// to be emulated, right? um... i'll want to ask our lecturer this, though 
// chances are we'll actually be getting to this in a few weeks.

//
// ARRAYS
// 

// Length of an array is how many items. In arrays, order matters (unlike objects).



// snippet for reversing strings, because we apparently don't have a builtin
// for this, exactly...
var reversed
  , name = "zac anger";

reversed = name.split("").reverse().join("");
return(reversed);

//
// THIS
//

// `this` is not assigned a value until an *object* invokes the *function*
// where `this` is defined.
// So, while it seems (to me, and I guess to others) that `this` refers to the
// object, it's not until an object invokes a function that `this` is
// actually assigned a value. The actual *value* is __only__ based on which
// object invoked said function. `this` has the value of the invoking object
// in most cases. sometimes it does not.

// IMPLICIT is the value attached to a keyword when it refers to an object
// and it applies to the PARENT object
//
// EXPLICIT (call, apply, bind)
//
// DEFAULT (window object)
//
// NEW in creation of new objects

// To mantain `this` inside anonymous functions, we can set the value
// before entering all that forEach bidnez.
// eg var that = this ... lol

var arrOne = [1, 44, 7, 89];
var arrTwo = [44, 2, 19, 21, 37, 1];
var arrThree = [56, 19, 44, 1, 12, 89];
function arrayItUp(arrOne, arrTwo, arrThree) {
  for (var i = 0; i => arrOne.length; i++) {
    for (var j = 0; j => arrTwo.length; j++) {
      for (var k = 0; k => arrThree.length; k++) {
        if (arrOne[i] === arrTwo[j] && arrOne[i] === arrThree[k]);
        arrFour.push(arrOne[i]);
      }
    }
  }
  return arrFour;
}
arrayItUp(arrOne, arrTwo, arrThree);

// all of this could've just been basically a foreach loop... and avoided
// the scope problems... crap.


//
// back to `this`
//
// because it's better to keep methods and their objects/properties together in
// the same model, one can always go ahead and make a method over there, and
// then include that method as the value to a key under the object. that way
// you've got modularity of a sort, but also keeping your models together in
// one place.
// so defining it in one place and then pointing to it in a second and maybe
// invoking it in a third? it's still going to point to that _function_ that
// called it (or, rather, that function's object).
// may be important, because he's said this several times now:
// 'we want our models to represent the real world.' and...
// 'we want the behaviors that represent our models to be with those models.'
// i suppose this is more expressive? either way, it's obvious how it'd be
// much easier to maintain and scale.
//
// .call is ONLY on a function, yo.
// .apply works identically to call, except in how one passes in parameters.
// first argument is *always* what `this` is bound to... which to use depends
// on how you intend to pass in parameters.
// btw, `arguments` is actually a keyword, apparently--for use in functions,
// it provides an array of all the passed parameters.
// but with .call, the second argument is a comma sep list;
// .call and .apply invoke the function immediately. .bind dooesn't. instead,
// it kind of makes a new function for you. that's actually super cool!
// OKAY OKAY wait. so. both call and apply are functions  that can ONLY be
// called on other functions! The one difference here is that call accepts both
// a first argument AND ANY ADDITIONAL ARGUMENTS THA ARE PASSED TO IT.
//
// okay, here's a new example.

var car = {};

function (doors, model, make) {
	this.doors = doors,
	this.make = make,
	this.model = model
	return
}

car.call (car, 2, 'Hyundai', 'Equus');

// so this is actually just a super roundabout way (in this simplified example) to do a constructor functon, i guess.


// okay, so the NEW keyword! var blah = new Blah(thing2 thing2);
// but javascript IMPLICITLY does that, and IMPLICITY returns things for you,
// and so makes things easier for you. HOWEVER, `new` is actually a shortcut
// for you. but don't do that. it's not pretty, and it's not nice.
// there's a thing that's like if (this instanceof Foo) or so, which will aid
// in avoiding using the window/browser/global object. syntax is super fuzzy
// though.
//
// currying: providing missing paramaters, some or all, when you call BIND.
// it will not override anything. so:

function what(one, two) {
	console.log('oi! ' + one);
}

var huh = { meh: 'yo' }; meh.what(); } (two);

// so with apply, those other parameters after the first, you'd better get
// them into an array.
//
// from rey:
// The this Keyword
// In JavaScript, the thing called this, is the object that "owns" the JavaScript code.
//The value of this, when used in a function, is the object that "owns" the function.
//The value of this, when used in an object, is the object itself.
//The this keyword in an object constructor does not have a value. It is only a substitute for the new object.
//The value of this will become the new object when the constructor is used to create an object.
//Note     Note that this is not a variable. It is a keyword. You cannot change the value of this.
//
//http://www.codeproject.com/Articles/857357/Introduction-to-HTML-WebSockets , https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API and http://www.html5rocks.com/en/tutorials/websockets/basics/ are some good primers on the websockets topic mentioned a little bit ago; and https://www.pubnub.com/blog/websockets-vs-rest-api-understanding-the-difference/ and https://news.ycombinator.com/item?id=3636681 for differences between websockets and http/rest/ajax/xhr/every-other-hacker-news-buzzword.
//
//
// ASYNC
//
// to avoid problems with async functions and synchronous functions causing conflicts
// (eg a sync one returning undefined because the async one hasn't finished yet),
// just do callbacks. that makes __sense__, okay!
//
// worth noting again that setTimeout really isn't fantastic practice, so maybe just don't do that...
// but if you do, it's super not difficult, so don't make it difficult.
// just put in a function (which can obviously just be calling something defined elsewhere), and the
// time (in milliseconds, which can also be calculated elsewhere). that's all.

function doThatThing(arr){
	return ('Hello ' + arr + ', how are you?');
}
setTimeout(doThatThing, 1000);

// ask:
// would it be harmful to think of underscore (or functional javascript in general) as
// being modular libraries/modules? (because javascript, at least in the browser, is not
// inherently modular -- no more so than it is inherently functional, object-oriented,
// or anything else... it's dynamic and multi-paradigm).

// order.updateTotal();
// console.log(order.total.bind();

// he uses 'self'; we could just as easily use 'that' or whatever `var foo = this.bar`
// ... or we could not, also.
//
// Functions as parameters

// Create a function that calculates 10% tax
function calculateTax(amount) {
  return amount * 0.10;
}

// Create a function that takes in an order amount calculates total
// including tax
function calculateTotal(amount) {
  return amount + calculateTax(amount);
}

var orderTotal = calculateTotal(10.00);
console.log(orderTotal);

// Create a function that calculates 7% tax for Utah
function calculateUtahTax(amount) {
  return amount * 0.07;
}

// Update calculateTotal to include a state parameter use
// the new utah calculator when the state is Utah
calculateTotal = function (amount, state) {
  if (state === 'Utah') {
    return amount + calculateUtahTax(amount);
  } else {
    return amount + calculateTax(amount);
  }
};

orderTotal = calculateTotal(20, 'Utah');
console.log(orderTotal);

// Q: What are some potential problems with this method?
// Q: What if tax calculation for Utah is based on county?
// Q: What if we need to calculate for other states?

// Update the calculateTotal to accept a function
// as a parameter that can calculate tax
calculateTotal = function (amount, taxCalculator) {
  return amount + taxCalculator(amount);
};


// Invoke calculateTotal with both calculateTax functions
orderTotal = calculateTotal(30, calculateUtahTax);
console.log(orderTotal);

// Q: What is better about this approach?


// Create a another function called GetTaxCalculator
// that takes state as a parameter and returns the
// correct tax calculator
function GetTaxCalculator() {
  
}

// Q: If we add new tax calculators, what code will need to be updated?

// Using functions as arguments to other functions is one way we can
// get results from an asynchronous function.

// Asynchronous & functions as callbacks

// Create a function called done, that writes done to the console
function done() {
  console.log('done');
}

// Use setTimeout and pass the done function as the callback
setTimeout(done, 2000);

console.log('after setTimeout');

// Q: Why does 'after setTimeout' display before 'done'?


// Create a function called getColors that uses setTimeout to simulate
// an asynchronous call to a web server that returns an array of colors
var getColors = function () {
  setTimeout(function () {
    //console.log('Returning colors...');
    return ['Red','Blue','Purple','Black'];
  },500);
};


// Q: What will the value of colors be? Why?
var colors = getColors();
console.log('Colors: ' + colors);

// Update getColors to solve the problem by using a callback
getColors = function (displayColors) {

  setTimeout(function () {
     displayColors(['Red','Blue','Purple','Black']);   
  },500); 

};
// Invoke getColors again using the callback
var displayColors = function (colors) {
  console.log(colors);
};

getColors(displayColors);
console.log('after getColors');


// Q: What challenges does asynchronous code create? How does it affect
// code readability?
// Q: What advantages does it provide?

// Create a function called forEach that takes an array as the first parameter, and a function as the second parameter.  The forEach function should loop over the items in the array and invoke the callback function, passing in each item in the array
var forEach = function (items, callback) {
  for (var i = 0; i < items.length; i++) {
    callback(items[i]);
  }
};

// Create an array of colors
var colors = ['Red', 'Blue', 'Purple', 'Pink'];

// use forEach to loop over the array of colors.  Pass a callback
// to forEach that receives a color as a parameter and outputs the
// name of the color to the console.
forEach(colors, function (color) {
  return color;
});

// Now create an order object that has a total property, and a property called items that is an array containing item prices. Add a method called updateTotal that uses the forEach function to loop over the items and update the total property
var order = {
  total: 0,
  items: [1.99,2.50,9.99],
  updateTotal: function () {
    this.total = 0;

    forEach(this.items, function (item) {
      this.total += item;
    });
  }
};

// Invoke the updateTotal method and display the order total
order.updateTotal();
console.log(order.total);

// Q: Why is the total zero?

// Change the updateTotal method to solve the problem using 'self'
order.updateTotal = function () {
  this.total = 0;

  var self = this;
  forEach(this.items, function (item) {
    self.total += item;
  });
};

order.updateTotal();
console.log("Total using 'self': " + order.total);

// Change the updateTotal method to solve the problem using 'bind'
order.updateTotal = function () {
  this.total = 0;

  forEach(this.items, function (item) {
    this.total += item;
  }.bind(this));
};

order.updateTotal();
console.log("Total using 'bind': " + order.total);

// Change the updateTotal method to solve problem by passing 'this'
// to forEach
// This solution requires updating our forEach function itself and isn't
// always an option

// Update forEach to take a 3rd parameter, which is what 'this' should
// be bound to
forEach = function (items, callback, whatThisShouldBe) {
  for (var i = 0; i < items.length; i++) {
    callback.call(whatThisShouldBe, items[i]);
  }
};

// Update updateTotal to pass it the order object
order.updateTotal = function () {
  this.total = 0;

  forEach(this.items, function (item) {
    this.total += item;
  }, this);
};

order.updateTotal();
console.log('Total using update to forEach: ' + order.total);


//
//
// JQUERY
//
//

// things to keep in mind:
// css and images will load up to 6 in parl
// js will only load in order, once after another
// because obviously some things depend on other things
//
//
.find // finds all below
.children // finds only direct immediate descendant
// this is like nesting in css selectors, basically


// dragndrop in jquery:

.on('mousedown', function() {
	isPressed = true;
}

.on('mouseup', function(){
	isPressed = false;
})

.on('mouseenter', function(){
	if(isPressed) {
		$(this).addClass(color);
	}
})

// assuming we're using that paint example, obvs.

// notes on localstorage:
localStorage.setItem('key', 'value'); ==
	localStorage.key = 'value'; ==
	localStorage['key'] = 'value';

return localStorage.key; ==
	return localStorage.getItem('key'); ==
	return localStorage['key'];

delete localStorage.key also works.
;;;;;;;;

//
//
//
// AJAX THINGS
//
//
// make data
// change data
// check data
// move data
// delete data
//
// is there anything else in an application but these five things?
// this is a dare, from jeremy. hmmmmmmmmmmm.
//
// notes: jeremy says to not store drug money data on PCs xD
// (because everything is data, data, data, data, data)
//
// Create - POST
// Read	  - GET
// Update - PUT
// Delete - DELETE
//
var ajaxParams = {
	method: 'GET',
	url: 'http://zacanger.com/blog/feed.rss',
	
}


$.ajax(ajaxParams).then(function(response){
	

})

// cyclomatic complexity
// more things you're doing
// more bugs
//
