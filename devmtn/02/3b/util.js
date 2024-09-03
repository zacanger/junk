/*jshint eqnull:true, expr:true*/
var _ = {}

;(function(){

  /**
   * COLLECTIONS
   * ===========
   *
   * In this section, we'll have a look at functions that operate on collections
   * of values; in JavaScript, a 'collection' is something that can contain a
   * number of values--either an array or an object.
   */

  // Return an array of the first n elements of an array. If n is undefined,
  // return just the first element (not in an array)
  _.first = function(array, n){
    var newArr = []
    if(!n){
      return array[0]
    } else if(n > array.length){
      n = array.length
    }
    for(var i = 0; i < n; i++){
      newArr.push(array[i])
    }
    return newArr
  }

  // Like first, but for the last elements. If n is undefined, return just the
  // last element.
  _.last = function(array, n){
    if(!n){
      return array[array.length - 1]
    } else if(n > array.length){
      n = array.length
    }
    return array.slice(array.length - n, array.length)
  }

  // Call iterator(value, key, collection) for each element of collection.
  // Accepts both arrays and objects.
  _.each = function(collection, iterator){
    for(var key in collection){
      var val = collection[key]
      iterator(val, key, collection)
    }
  }

  // Returns the index at which value can be found in the array, or -1 if value
  // is not present in the array.
  _.indexOf = function(array, target){
    for(var i = 0; i < array.length; i++){
      if(array[i] === target){
        return i
      }
    }
    return -1
  }

  // Return all elements of an array that pass a truth test (i.e., return truthy from the iterator function argument)
  _.filter = function(collection, iterator){
    var filteredArr = []
    for(var key in collection){
      if(iterator(collection[key])){
        filteredArr.push(collection[key])
      }
    }
    return filteredArr
  }

  // Return all elements of an array that don't pass a truth test (i.e., return falsey from the iterator function argument)
  _.reject = function(collection, iterator){
    var rejectedArr = []
    for(var key in collection){
      if(!iterator(collection[key])){
        rejectedArr.push(collection[key])
      }
    }
    return rejectedArr
  }

  // Produce a duplicate-free version of the array.
  _.uniq = function(array){
    var uniqArr = []
    for(var i = 0; i < array.length; i++){
      if(uniqArr.indexOf(array[i]) === -1){
        uniqArr.push(array[i])
      }
    }
    return uniqArr
  }

  // Return the results of applying an iterator to each element.
  _.map = function(array, iterator){
    var resultArr = []
    for(var i = 0; i < array.length; i++){
      resultArr.push(iterator(array[i]))
    }
    return resultArr
  }

  // Takes an array of objects and returns and array of the values of
  // a certain property in it. E.g. take an array of people and return
  // an array of just their ages
  _.pluck = function(array, propertyName){
    var valArr = []
    for(var i = 0; i < array.length; i++){
      valArr.push(array[i][propertyName])
    }
    return valArr
  }

  // Calls the method named by methodName on each value in the list.
  // List is an array of objects (could be array of arrays, calling build in array methods, e.g. array.sort())
  // Returns array of results of calling method on each item
  _.invoke = function(list, methodName, args){
    var invokeArr = []
    for(var i = 0; i < list.length; i++){
      // list[i][methodName]()
      invokeArr.push(list[i].sort())
    }
    return invokeArr
  }

  // Reduces an array or object to a single value by repetitively calling
  // iterator(previousValue, item) for each item. previousValue should be
  // the return value of the previous iterator call.
  _.reduce = function(collection, iterator, initialValue){
    var value
    if(initialValue){
      value = initialValue
    } else {
      value = 0
    }
    for(var key in collection){
      value = iterator(value, collection[key])
    }
    return value
  }

  // Determine if the array or object contains a given value (using `===`).
  // Collection param is either a array or an object, not a 'collection' of objects
  // Changing parameter name to list
  _.contains = function(list, target){
    if(Array.isArray(list) && list.indexOf(target) > -1){
      return true
    }
    for(var key in list){
      if(list[key] === target){
        return true
      }
    }
    return false
  }

  // Determine whether all of the elements match a truth test.
  _.every = function(collection, iterator){
    if(!iterator){
      iterator = function(item){
        if(item){
          return true
        } else {
          return false
        }
      }
    }
    if(Array.isArray(collection)){
      for(var i = 0; i < collection.length; i++){
        if(!iterator(collection[i])){
          return false
        }
      }
    }
    for(var key in collection){
      if(!iterator(collection[key])){
        return false
      }
    }
    return true
  }

  // Determine whether any of the elements pass a truth test. If no iterator is
  // provided, provide a default one
  _.some = function(collection, iterator){
        // if iterator not present, iterator becomes basic truthy/falsy test
    if(!iterator){
      iterator = function(item){
        if(item){
          return true
        } else {
          return false
        }
      }
    }
    if(Array.isArray(collection)){
      for(var i = 0; i < collection.length; i++){
        if(iterator(collection[i])){
          return true
        }
      }
    }
    for(var key in collection){
      if(iterator(collection[key])){
        return true
      }
    }
    return false
  }


  /**
   * OBJECTS
   * =======
   *
   * In this section, we'll look at a couple of helpers for merging objects.
   */

  // Extend a given object with all the properties of the passed in
  // object(s).
  // Returns updated object

  // function sortArgs() {
  //   var args = Array.prototype.slice.call(arguments);
  //   return args.sort();
  // }

  _.extend = function(obj){
    for(var i = 1; i < arguments.length; i++){
      for(var key in arguments[i]){
        obj[key] = arguments[i][key]
      }
    }
    return obj
  }

  // Like extend, but doesn't ever overwrite a key that already
  // exists in obj
  _.defaults = function(obj){
    for(var i = 1; i < arguments.length; i++){
      for(var key in arguments[i]){
        if(!obj.hasOwnProperty(key)){
          obj[key] = arguments[i][key]
        }
      }
    }
    return obj
  }


  /**
   * FUNCTIONS
   * =========
   */

  // Return a function that can be called at most one time. Subsequent calls
  // should return the previously returned value.
  _.once = function(func){
    var hasCalled = false
    var funcValue
    return function(){
      if(!hasCalled){
        hasCalled = true
        funcValue = func.apply(this, arguments)
      }
      return funcValue
    }
  }

  // Memoize an expensive function by storing its results. You may assume
  // that the function takes only one argument and that it is a primitive.
  // Memoize should return a function that when called, will check if it has
  // already computed the result for the given argument and return that value
  // instead if possible.
  _.memoize = function(func){
    var cache = {}
    // function to create key for function, based on its arguments. Returns the argument passed as key.
    var funcVal = function(value){
      return value
    }
    return function (){
      var key = funcVal.apply(this, arguments)
      if(key in cache){
        return cache[key]
      } else {
        var val = func.apply(this, arguments)
        cache[key] = val
        return val
      }
    }
  }

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  // The arguments for the original function are passed after the wait
  // parameter. For example _.delay(someFunction, 500, 'a', 'b') will
  // call someFunction('a', 'b') after 500ms
  _.delay = function(func, wait){
    var args = Array.prototype.slice.call(arguments, 2) // gets arguements parameter (third)
    return setTimeout(function(){ // returns the function invocation after x ms
      return func.apply(null, args) // returns the function invoked with
    }, wait)
  }

  // Shuffle an array.
  _.shuffle = function(array){
    var newArr = []
    while (array.length > 0){
      var rand = Math.floor((Math.random() * array.length))
      newArr.push(array.splice(rand, 1)[0])
    }
    return newArr
  }

  // Sort the object's values by a criterion produced by an iterator.
  // If iterator is a string, sort objects by that property with the name
  // of that string. For example, _.sortBy(people, 'name') should sort
  // an array of people by their name.
  // collection: either array of values or array of objects
  // iterator: function or string
  // undefined values at end

  _.sortBy = function(collection, iterator){
    var compareFunc
    if(typeof iterator === 'string'){
      compareFunc = function(a, b){
        if(a[iterator] < b[iterator]){
          return -1
        } else if(a[iterator] > b[iterator]){
          return 1
        } else {
          return 0
        }
      }
    } else if(typeof iterator === 'function'){
      compareFunc = function(a, b){
        if(iterator(a) < iterator(b)){
          return -1
        } else if(iterator(a) > iterator(b)){
          return 1
        } else {
          return 0
        }
      }
    }
    collection.sort(compareFunc)
    return collection
  }

  // Zip together two or more arrays with elements of the same index
  // going together.
  // _.zip(['a','b','c','d'], [1,2,3]) returns [['a',1], ['b',2], ['c',3], ['d',undefined]]
  _.zip = function(){
    var zippedArray = []
    var subArr = 0
    for(var i = 0; i < arguments.length; i++){
      var currLen = arguments[i].length
      if(currLen > subArr){
        subArr = currLen
      }
    }
    for(var j = 0; j < subArr; j++){
      zippedArray.push([])
      for(var k = 0; k < arguments.length; k++){
        zippedArray[j].push(arguments[k][j])
      }
    }
    return zippedArray
  }

  // Takes a multidimensional array and converts it to a one-dimensional array.
  // The new array should contain all elements of the multidimensional array.
  _.flatten = function(nestedArray, result){
    var numArr = []
    var allNums
    for(var i = 0; i < nestedArray.length; i++){
      if(typeof nestedArray[i] !== 'number'){
        allNums = false
        break
      } else {
        allNums = true
      }
    }
    if(allNums){
      Array.prototype.push.apply(numArr, nestedArray)
      return numArr
    } else {
      var levelArr = []
      for(var j = 0; j < nestedArray.length; j++){
        var item = nestedArray[j]
        if(typeof item === 'number'){
          levelArr.push(item)
        } else {
          Array.prototype.push.apply(levelArr, this.flatten(nestedArray[j]))
        }
      }
      return levelArr // return the flattened level array
    }
  }

  // Takes an arbitrary number of arrays and produces an array that contains
  // every item shared between all the passed-in arrays.
  _.intersection = function(){
    var countItems = {}
    for(var i = 0; i < arguments.length; i++){
      var argArray = arguments[i]
      for(var j = 0; j < argArray.length; j++){
        var currItem = argArray[j]
        if(countItems[currItem]){
          countItems[currItem]++
        } else {
          countItems[currItem] = 1
        }
      }
    }
    var sharedItems = []
    for(var itemProp in countItems){
      if(countItems[itemProp] === arguments.length){
        sharedItems.push(itemProp)
      }
    }
    return sharedItems
  }

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  _.difference = function(array){
    for(var i = 1; i < arguments.length; i++){
      var currArray = arguments[i]
      for(var j = 0; j < currArray.length; j++){
        var item = currArray[j]
        var itemIndex = array.indexOf(item)
        if(itemIndex > -1){
          array.splice(itemIndex, 1)
        }
      }
    }
    return array
  }
}).call(this)

