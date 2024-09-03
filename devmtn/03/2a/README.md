# User Profiles
### Understanding Services

## Objective
### To better understand the relationship between Angular controllers and services.

## Step 1 - Basic setup
- Create and setup an index.html page
- Insert the AngularJS CDN: http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.3.3/angular.min.js
- Create an app.js file
- Create a controller.js file
- Create a service.js file

## Step 2 - index.html
- Remember the basic index.html layout
- Then link in our other files
  - Remember to load JavaScript files at the bottom of the body
- Include the app.js, controller.js, and service.js file, as well as the CDN
  - Remember that the CDN must go on top of your other files or your app will throw an error 
- Next we will add some things that tell our index that we are working with AngularJS
  - ng-app loads our primary angular module. Let's name the module 'userProfiles'
  - ng-controller loads our controller for the view. Let's name the controller 'MainController'


## Step 3 - Our Basic JS Files
- Set up app.js
  - This is where we will create our module 
- REMEMBER: when we put "[]" in our angular.modules declaration, it's telling Angular that we are creating a new module named "userProfiles". If we were to omit the "[]" it would be asking Angular to go and look for a module named "userProfiles"
- Set up controller.js
  - Create a new controller, pointing to our 'userProfiles' app (or whatever you named the app).
- Lastly, we will create our Service in a similar fashion:
- We will also want to load some data into our service. Copy the following data into the service:
``` json
{
    "id": 1,
    "first_name": "george",
    "last_name": "bluth",
    "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/calebogden/128.jpg"
},
{
    "id": 2,
    "first_name": "lucille",
    "last_name": "bluth",
    "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/josephstein/128.jpg"
},
{
    "id": 3,
    "first_name": "oscar",
    "last_name": "bluth",
    "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/olegpogodaev/128.jpg"
}
```

So now our service should look like this:

``` javascript
var app = angular.module('userProfiles');

app.service('mainService', function() {
  var data = 
  [
    {
        "id": 1,
        "first_name": "george",
        "last_name": "bluth",
        "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/calebogden/128.jpg"
    },
    {
        "id": 2,
        "first_name": "lucille",
        "last_name": "bluth",
        "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/josephstein/128.jpg"
    },
    {
        "id": 3,
        "first_name": "oscar",
        "last_name": "bluth",
        "avatar": "https://s3.amazonaws.com/uifaces/faces/twitter/olegpogodaev/128.jpg"
    }
  ]
});
```

## Step 4 - Our Service
Our Service will do most of the apps heavy lifting. We want to keep our controllers as slim as possible. To do that we will need to create a function that delivers our data to our controller.
- Write a function called "getUsers" that will return all of our user data to the controller
  - Remember: functions made in a service can be tied to the service via the "this" keyword 

This new function allows us to access the variable "data" outside of the service file.

## Step 5 - Our Controller
The next thing we need to do is to create a function in our controller that gathers the data and prepares it to be sent to the view.

- In the controller.js file, create a function on the $scope object named "getUsers"
- Then, inject the mainService in the controller
- Now, within the new getUsers function, we can access the mainService's getUsers function
  - Let's set a variable called $scope.users equal to the result of the mainService's function
  - Also, we should invoke our $scope.getUsers function after we have declared it or it won't run

Now we have an object named "$scope.users" which represents our data. Because it is on the $scope object we can access it in our view by placing this within the body of our index.html, using `{{}}` binding.


# Step 6 - The View
Now we have our data in our view, but it's a little ugly. Let's do some simple configuration to make it a bit more userfriendly. Typically when you have an array of data, it's a good idea to use ng-repeat to organize it.

Now we should have some awesome user profiles! 
