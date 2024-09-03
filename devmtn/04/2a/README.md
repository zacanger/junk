mini-timeDirective
==================

##Objectives
The purpose of this Mini Project is to get you used to creating custom Directives. Custom directives are one of the most powerful aspects of Angular because they allow you to create re-usable components.

###Step 1: Angular Skeleton 
* Fork this repo, then clone your fork.
* Create the basics of your Angular application. Your file structure should look like this
```
  mini-routing
    index.html
    js
      app.js
      mainCtrl.js
      timeDirective.js
```
Remember to include ng-app in your html and call your module 'timeApp'. Also, remember to include the Angular CDN as a script in your HTML along with app.js, mainCtrl.js, and timeDirective.js. Go ahead and create your 'timeApp' module in your app.js file. 

###Step 2: Add your name to $scope
* Head over to your mainCtrl.js file and create a controller called mainCtrl then add a property on $scope called name and set it equal to your name.
* Head over to your index.html and add 'Hello, ' + whatever the name property on your controller is. 

###Step 3: Directive Time
* Below where it says 'Hello, ' + your $scope.name variable, add the following directive ```<show-time></show-time>```
* This directive is going to display 'The current time is ' + whatever the current time is. Although this is a small example, think of the bigger picture here. Now we can throw in this ```<show-time></show-time>``` directive anywhere in our application without having to recreate code.
* Now it's time to actually build the directive. Double check that you're not getting any errors in your console before we move on.
* Head over to timeDirective.js
* Go ahead and get your module, setting it equals to a variable called 'app'
* Now, add a directive property onto your app giving it a string, which represents  the name of your directive, call it 'showTime' as the first argument. The second argument is a callback function that will return an object. The skeleton of your directive should look something like this.
```javascript
  app.directive('showTime', function(){
  return {

  }
});
```
* Now what we want to do is give our directive certain properties. The first one is we want to make sure it's only used as an element, i.e., ```<show-time>```. You do this by setting ```restrict: 'E' ``` in the object that's being returned from the directives callback.
* The next property we want to give it is a template. This template is just an HTML string that will show wherever the directive is used. Add a property to the object being returned called 'template' whose value is ```<div> The current time is {{time}} </div>```
* What we're going to do now is use the link method to get the current time, then update scope.time which will in turn update the template.
* Add a method onto your returned object called link. Remember, this link method is where you want to put all your DOM manipulation for JavaScript. 
* The link value usually takes three parameters. scope, element, and attrs. Add those as parameters now
* Now create a variable called currentTime and set it equal to ```new Date()```, which is just the current time.
* Now, add a property on scope called time which is equal to  ```currentTime.toString```
* If you did everything right reload your index.html page and you should see something like
```
  Hello, Tyler
  The current time is Sun Dec 07 2014 22:03:12 GMT-0700 (MST)
```
* If you don't see that, check your console and start debugging.
* Your final timeDirective.js file should look like this
```javascript
var app = angular.module('timeApp');

app.directive('showTime', function(){
  return {
    restrict: 'E',
    template: '<div> The current time is {{time}} </div>',
    link: function(scope, element, attrs){
      var currentTime = new Date();
      scope.time = currentTime.toString();
    }
  }
});
```
