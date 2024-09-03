/*jshint eqnull:true, expr:true*/
var _ = {};

(function() {

  /**
   * COLLECTIONS
   * ===========
   *
   * In this section, we'll have a look at functions that operate on collections
   * of values; in JavaScript, a 'collection' is something that can contain a
   * number of values--either an array or an object.
   */

  // Return an array of the first n elements of an array. If n is undefined,
  // return just the first element.
  _.first = function(array, n) {

    if (n !== undefined) {
      return array.slice(0, n);
    } else {
      return array[0];
    }
  };

  // Like first, but for the last elements. If n is undefined, return just the
  // last element.[1,2,3,4,5]
  _.last = function(array, n) {
    if (n) {
      return array.reverse().slice(0, n).reverse();
    } else {
      return array[array.length - 1];
    }
  };

  // Call iterator(value, key, collection) for each element of collection.
  // Accepts both arrays and objects.
  _.each = function(collection, iterator) {
    if (Array.isArray(collection)) {
      for (var i = 0; i < collection.length; i++) {
        iterator(collection[i], i, collection);
      }

    } else {
      for (var prop in collection) {
        iterator(collection[prop], prop, collection);
      }
    }
    return collection;
  };

  // Returns the index at which value can be found in the array, or -1 if value
  // is not present in the array.
  _.indexOf = function(array, target) {

    for (var i = 0; i < array.length; i++) {

      if (array[i] === target) {
        return i;
      }
    }
    return -1;
  };

  // Return all elements of an array that pass a truth test.ele index array
  _.filter = function(collection, iterator) {
    var returnArray = [];
    for (var i = 0; i < collection.length; i++) {
      if (iterator(collection[i])) {
        returnArray.push(collection[i]);
      }
    }
    return returnArray;
  };

  // Return all elements of an array that don't pass a truth test.
  _.reject = function(collection, iterator) {
    var returnArray = [];
    for (var i = 0; i < collection.length; i++) {
      if (!iterator(collection[i])) {
        returnArray.push(collection[i]);
      }
    }

    return returnArray
  };

  // Produce a duplicate-free version of the array.
  _.uniq = function(array) {
    var returnArray = [];


    for (var i = 0; i < array.length; i++) {


      var want = array[i];
      var okay = true;

      for (var p = 0; p < returnArray.length; p++) {
        if (returnArray[p] == want) {
          okay = false;
        }

      }
      if (okay) {
        returnArray.push(want);
      }
    }
    return returnArray;
  };


  // Return the results of applying an iterator to each element.
  _.map = function(array, iterator) {
    var returnArray = [];
    for (var i = 0; i < array.length; i++) {
      returnArray.push(iterator(array[i]));

    }
    return returnArray;
  };

  // Takes an array of objects and returns and array of the values of
  // a certain property in it. E.g. take an array of people and return
  // an array of just their ages
  _.pluck = function(array, propertyName) {
    var returnedArray = [];

    for (var i = 0; i < array.length; i++) {
      returnedArray.push(array[i][propertyName]);
    }
    return returnedArray;
  };

  // Calls the method named by methodName on each value in the list.
  _.invoke = function(list, methodName, args) {

    if (typeof methodName === "string") {

      for (var i = 0; i < list.length; i++) {
        list[i][methodName](args);
      }

    } else {

      for (var i = 0; i < list.length; i++) {
        methodName.apply(list[i], args);
      }
    }
    return list;


  };




  // Reduces an array or object to a single value by repetitively calling
  // iterator(previousValue, item) for each item. previousValue should be
  // the return value of the previous iterator call.
  _.reduce = function(collection, iterator, initialValue) {
    // console.log(collection);

    if (initialValue) {

      for (var i = 0; i < collection.length; i++) {
        initialValue = iterator(initialValue, collection[i]);
      }
      return initialValue;

    } else {
      var initialValue = 0;
      for (var i = 0; i < collection.length; i++) {
        initialValue = iterator(initialValue, collection[i]);
      }
      return initialValue;
    }

  };

  // Determine if the array or object contains a given value (using `===`).
  _.contains = function(collection, target) {

    for (var i = 0; i < collection.length; i++) {
      if (collection[i] === target) {
        return true;
      }
    }
    for (var parts in collection) {
      if (collection[parts] === target) {
        return true;
      }
    }
    return false;
  };


  // Determine whether all of the elements match a truth test.
  _.every = function(collection, iterator) {

    if (iterator) {
      for (var i = 0; i < collection.length; i++) {
        if (!iterator(collection[i])) {
          return false;
        }
      }
    }

    return true;
  };




  // Determine whether any of the elements pass a truth test. If no iterator is
  // provided, provide a default one
  _.some = function(collection, iterator) {

    for (var i = 0; i < collection.length; i++) {

      if (iterator) {

        if (iterator(collection[i])) {
          return true;
        }
      }
      if (!iterator) {
        //strings are truthy, so cant set against an == true
        if (collection[i]) {

          return true;
        }
      }
    }
    return false;
  };


  /**
   * OBJECTS
   * =======
   *
   * In this section, we'll look at a couple of helpers for merging objects.
   */

  // Extend a given object with all the properties of the passed in
  // object(s).
  _.extend = function(obj) {

    for (var i = 1; i < arguments.length; i++) {

      for (var prop in arguments[i]) {
        obj[prop] = arguments[i][prop];
      }
    }
    return obj;
  };

  // Like extend, but doesn't ever overwrite a key that already
  // exists in obj


  _.defaults = function(obj) {

    for (var i = 1; i < arguments.length; i++) {

      for (var prop in arguments[i]) {
        if (!obj.hasOwnProperty(prop)) {
          obj[prop] = arguments[i][prop];
        }
      }
    }
    return obj;

  };


  /**
   * FUNCTIONS
   * =========
   */

  // Return a function that can be called at most one time. Subsequent calls
  // should return the previously returned value.
  _.once = function(func) {

    var solution = func();

    var doIt = function() {
      return solution;
    }

    return doIt;

  };


  // Memoize an expensive function by storing its results. You may assume
  // that the function takes only one argument and that it is a primitive.
  //
  // Memoize should return a function that when called, will check if it has
  // already computed the result for the given argument and return that value
  // instead if possible.
  _.memoize = function(func) {
    function testAndDo(num) {

      var pastResults = {};
      if (pastResults.hasOwnProperty(num)) {
        return pastResults[num];
      } else {
        pastResults.num = func(num);

        return func(num);
      }
    }
    return testAndDo;
  };

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  //
  // The arguments for the original function are passed after the wait
  // parameter. For example _.delay(someFunction, 500, 'a', 'b') will
  // call someFunction('a', 'b') after 500ms
  _.delay = function(func, wait) {

    var argumentsLength = arguments.length;
    var params = [];
    for (var i = 2; i < argumentsLength; i++) {
      params.push(arguments[i]);
    };
    return setTimeout(
      function() {
        func.apply(undefined, params);
      }, wait)

  };


  // Shuffle an array.
  _.shuffle = function(array) {
    var oldArray = array;
    var shuffledArray = [];
    for (var i = 0; i < array.length; i++) {
      var randomNum = Math.random() * (oldArray - 1)
      shuffledArray.push(oldArray[randomNum])
      oldArray = oldArray.splice(randomNum, 1);
    };
    return shuffledArray;

  };

  // Sort the object's values by a criterion produced by an iterator.
  // If iterator is a string, sort objects by that property with the name
  // of that string. For example, _.sortBy(people, 'name') should sort
  // an array of people by their name.
  _.sortBy = function(collection, iterator) {

    var answer = [];
    console.log(iterator);

    switch (typeof iterator) {

      case 'function':
        var test = 0
        var answer = [];
        for (var i = 0; i < collection.length; i++) {
          for (key in collection[i])
            if (iterator(collection[i]) > test) {
              test = iterator(collection[i]);
              console.log(collection[i]['name']);

              answer.unshift(collection[i].name)
            }
        }
        return answer;
        break;
      default:

    }


    for (var i = 0; i < collection.length; i++) {



      // }
    } //main for
    return answer;
  };




  // Zip together two or more arrays with elements of the same index
  // going together.
  //
  // Example:
  // _.zip(['a','b','c','d'], [1,2,3]) returns [['a',1], ['b',2], ['c',3], ['d',undefined]]
  _.zip = function() {
    var argsLength = arguments.length;

    var params = [];
    var longest = 0;
    var arraySorted = [];
    var returnableArray = [];

    for (var i = 0; i < argsLength; i++) {
      params.push(arguments[i]);
    }

    for (var i = 0; i < params.length; i++) {

      if (params[i].length > longest) {
        longest = params.length;
        arraySorted.unshift(params[i])
      } else {
        arraySorted.push(params[i]);
      }
    }

    for (var i = 0; i < longest; i++) {
      returnableArray[i] = [];

      for (var p = 0; p < arraySorted.length; p++) {
        returnableArray[i].push(arraySorted[p][i]);
      }
    }
    return returnableArray;
  };

  // Takes a multidimensional array and converts it to a one-dimensional array.
  // The new array should contain all elements of the multidimensional array.
  _.flatten = function(nestedArray, result) {

    var stringifiedThenArrayified = String(nestedArray).split(",")

    return stringifiedThenArrayified;

  };

  // Takes an arbitrary number of arrays and produces an array that contains
  // every item shared between all the passed-in arrays.[2,3,4], [2,6,4,2]
  _.intersection = function() {
    var common = [];
    var argLength = arguments.length;
    var args = [];
    for (var i = 0; i < argLength; i++) {
      args.push(arguments[i]);
    };

    for (var i = 0; i < args.length - 1; i++) {
      for (var target = 0; target < args[i].length; target++) {

        if (args[i].indexOf(args[i + 1][target]) !== (-1)) {

          common.push(args[i + 1][target]);
        } //if

      }
    } //i
    return common;

  };

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  _.difference = function() {



    var uncommon = [];
    var args = [];
    for (var i = 0; i < arguments.length; i++) {
      args.push(arguments[i]);
    };
    var main = args.shift();
    var alls = String(args).split(",").sort();
    var numAlls = [];
    for (var i = 0; i < alls.length; i++) {
      numAlls.push(Number(alls[i]));
    }
    for (var i = 0; i < main.length; i++) {
      if (numAlls.indexOf(main[i]) == (-1)) {

        uncommon.push(main[i]);
      }
    }

    return uncommon;
  };

}).call(this);