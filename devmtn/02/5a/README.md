# Understanding AJAX with jQuery

Today we're going to talk about Ajax, and how we can use jQuery to make Ajax requests.

Ajax allows us to retreive data from a server or API without refreshing the entire page. Before Ajax, the entire page would need to refresh in order to retreive new data.

Ajax has the helped web to become what it is today. Can you imagine making a facebook post and having it refresh the entire website?

If you look at the index.html of this app, we have three things. We have a form that takes in data, a 'get current users' section, and a 'recently added user' section.

The form will take in data, and then pass it to the 'recently added user' section. If we add a new user it will replace the previous one. The get current users section will hit an api and pull their users data.

We will be using the [reqres](http://reqres.in/) for our API, which is an open API filled with dummy data. Let's get started!

## Step 1 - Our first GET request

- Write some jQuery that handles the click event on our get current users section

``` javascript
  $('body').on('click', '.js-get-users', function () {
    // do stuff here
  })
```

- Now let's return the ajax request

``` javascript
  $('body').on('click', '.js-get-users', function () {
    return $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(handleData, handleError)
  })
```

We are telling our request 3 things:

  - What kind of request (method)
  - What is the URL
  - What do we do on a successful request

- Let's give our app something to do if the request is successful

``` javascript
  $('body').on('click', '.js-get-users', function () {

    function handleSuccess(res) {
      console.log(res);
    }

    $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(handleSuccess);
  })
```

Now when we click the button, we will end up with the response of our api in our console. Cool!

Being able to console.log data is cool, and a great place to start, but we need to get that data into our DOM!

- Create an insertData function that takes the data, parses our it's valuable information, and writes it to the DOM

``` javascript
  var insertData = function (arr) {
    var tpl = '<div>' +
        'User Info: <ul>' +
        '<li>First name: <span class="js-first">none</span></li>' +
        '<li>Last name: <span class="js-last">none</span></li>' +
        '</ul>' +
        '<hr>' +
        '</div>';

    arr.forEach(function (item, i) {
      var $copy = $(tpl);

      $copy.find('.js-first').text(item.first_name);
      $copy.find('.js-last').text(item.last_name);

      $('.js-user-info-' + (i + 1)).html($copy);
    });
  }
```

What this does is take the data, iterates through it with a loop and writes it into the DOM. It's not pretty, but it gets the job done.


- Call the insertData function with the data we recieved.

``` javascript
  $('body').on('click', '.js-get-users', function () {
    return $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(function (res) {
      console.log(res);
      insertData(res.data);
    }, function (err) {
      console.error(err);
    });
  })
```

Now when we click the GET current users button we should see them populate in our DOM.

## Step 2 - Our first POST request

GET requests are the easiest of requests. POSTs are a bit more tricky, but not by a lot. Let's hook our form up so that we can make a post request!

- Hook up the form submit so that it is ready to handle the submit event without causing a page reload
- submit events happen when
  - you click a button (unless it has type="button")
  - you click an input with type="submit"
  - you press enter

``` javascript
  $('body').on('submit', '.js-add-user', function (ev) {
    // By default a form submission will cause a page to reload
    // but it is possible to tell the event to cancel it's default action like this
    ev.preventDefault();

    // do thing here
  });
```

- The next thing we want to do is capture the value of our input forms using .val()

``` javascript
  $('body').on('click', '.js-add-user', function () {
    var userName = $('.js-name').val();
    var userJob = $('.js-job').val();
  });
```

.val() gives us the value of something. Once we've filled out the form, the value of the input fields are what we typed into it.

- Have the function return our ajax POST request

``` javascript
  $('body').on('click', '.js-add-user', function () {
    var userName = $('.js-name').val();
    var userJob = $('.js-job').val();
    return $.ajax({
      method: 'POST',
      url: 'http://reqres.in/api/users',
      data: {name: userName, job: userJob}
    })
  });
```

Notice how our method now says post, and our URL is a little different. Another thing that is different is this 'data' part. Data lets us pass specific information to the API via our request. This is useful when making POST requests because we are POSTing something to the api. It's also helpful when trying to find specific user data, we can do something like pass in the users ID.

Our data is currently the values from our input fields.

- Let's add success and error handlers to our request.

``` javascript
  $('body').on('click', '.js-add-user', function () {
    var userName = $('.js-name').val();
    var userJob = $('.js-job').val();

    $.ajax({
      method: 'POST',
      url: 'http://reqres.in/api/users',
      data: {name: userName, job: userJob},
    }).then(function (res) {
      // TODO on success
    }, function (error) {
      // TODO on error
    });
  });
```

- Inside our success function, we need to make some html we can add into our DOM.
- Inside our error function, we'll alert the user that something went wrong.

``` javascript
  $('body').on('click', '.js-add-user', function (e) {
    e.preventDefault();
    var userName = $('.js-name').val();
    var userJob = $('.js-job').val();
    return $.ajax({
      method: 'POST',
      url: 'http://reqres.in/api/users',
      data: { name: userName, job: userJob },
    }).then(function (res) {
      var tpl = '<li>name: <span class="js-name">none</span></li>' +
        '<li>job: <span class="js-job">none</span></li>' +
        '<li>id: <span class="js-id">none</span></li>' +
        '<li>created at:  <span class="js-created-at">none</span></li>'
        ;

      $copy = $(tpl);
      $copy.find('.js-name').text(res.name);
      $copy.find('.js-job').text(res.job);
      $copy.find('.js-id').text(res.id);
      $copy.find('.js-created-at').text(res.createdAt);

      $('.js-recent-user').html($copy);
    }, function (err) {
      console.error(err);
      window.alert('Something went wrong!');
    });
  });
```

Congrats!  You've just created your first CRUDdy app!

