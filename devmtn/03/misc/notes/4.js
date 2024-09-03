$q is angular's implementation of promises.


var myDeferred = $q.defer();

async(function(val) {
  myDeferreed.resolve(val);
}, function(error) {
  myDeferred.reject(error);
});


async(myDeferred.resolve, myDeferred.reject);
//
var myPromise = myDeferred.promise;
myPromise
  .then(function(data) {
    console.log('It worked!', data);
}, function(error) {
    console.log('NOPE', error);
});

can assign success/failure ops right after creating the deferred (before async()), and can assign as many ops as we like.

we can also assign ONLY success or failure functions:

promise.then(function() {
  console.log('assigning only success here')'
});

promise.catch(function () {
  console.log('assigning only failure here');
}); // this is the same as typing out promise.then(null.errorCallback)


promise.finally(function() {
  console.log('on, and this one will happen whether we fail or succeed.')
});

can chain promises (as values), for example asyncTwo(asyncOne), but like:
var promise = asyncOne()
  .then(asyncTwo);

$q.reject(reason) will return rejected, with provided reason
$q.when(value) will return resolved, with provided value.
$q.when(value) can serve as a wrapper for a 3rd party promise. example:
$.ajax() //jQuery
var jqPromise = $.ajax({
// stuff here });
var angPromise = $q.when(jqPromise);

$q.all( [] );
in the above, one passes an array of promises to $q.all

//

angular flow in ten seconds or less:
this is according to luke, so it's probably correct.
$http.get(url)
promise.then on that
to display it, call that function on the service
return that, $scope.that thing
{{let's go ahead and display that.thing however we want that}}

//

angular hits serious view bottleneck around 1800 items.

node's event loop is exactly the same as normal async javascript, just not in a browser

//

$q.defer() // making a promise
$q.resolve() // start resolving that promise, start invoking chain of .then callback functions,
             // aka 'i'm done and it worked!'

doThing(){
  function doThing(){
    var promise = $q.defer;
    // stuff to do
    // this needs a callback, probably
      function callback(){
        promise.resolve(['stuff', 'that you got', 'because you asked for it']);
      }
    return promise;
  };
}


var url = 'http//zacnger.com/blog";
var promise = $q.defer();
$http({
  method: 'GET'.
  url: url
}).then(function(response){
  var whatever = response.data;
  // do things
});

okay, promises are so much simpler and more straightforward than people are probably thinking.
really, basically, we're just going over use-cases and examples now. which is totally fine,
of course.

note that when we go ahead and send a buncha chained promises, we're gonna get stuff back
however it comes back, not in the order we started things. we'd want to sort things after,
if needed, not (obviously) within promises, because that would make all of that redundant;
we'd basically be using promises to run some synchronous javascript, at that point.
dumb.

JSONP: this is how we get around cross-origin problems.
