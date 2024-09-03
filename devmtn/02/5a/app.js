$(document).ready(function() {
  $('body').on('click', '.js-get-users', function() {
    return $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(handleData, handleError)
  })
  $('body').on('click', '.js-get-users', function () {

    function handleSuccess(res) {
      console.log(res);
    }

    $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(handleSuccess);
  });

  var insertData = function(arr) {
    var tpl = '<div>' + 'User Info: <ul>' + '<li>First name: <span class="js-first">none</span></li>' +
      '<li>Last name: <span class="js-last">none</span></li>' + '</ul>' + '<hr>' + '</div>';
    arr.forEach(function(item, i) {
      var $copy = $(tpl);
      $copy.find('.js-first').text(item.first_name);
      $copy.find('.js-last').text(item.last_name);
      $('.js-user-info-' + (i + 1)).html($copy);
    });
  }
  $('body').on('click', '.js-get-users', function() {
    return $.ajax({
      method: 'GET',
      url: 'http://reqres.in/api/users?page=1',
    }).then(function(res) {
      console.log(res);
      insertData(res.data);
    }, function(err) {
      console.error(err);
    });
  });

  $('body').on('click', '.js-add-user', function(ev) {
    ev.preventDefault();
    var userName = $('.js-name').val();
    var userJob = $('.js-job').val();
    return $.ajax({
      method: 'POST',
      url: 'http://reqres.in/api/users',
      data: {
        name: userName,
        job: userJob
      }
    }).then(function(res) {
      var tpl = '<li>name: <span class="js-name">none</span></li>' +
        '<li>job: <span class="js-job">none</span></li>' + '<li>id: <span class="js-id">none</span></li>' +
        '<li>created at:  <span class="js-created-at">none</span></li>';
      $copy = $(tpl);
      $copy.find('.js-name').text(res.name);
      $copy.find('.js-job').text(res.job);
      $copy.find('.js-id').text(res.id);
      $copy.find('.js-created-at').text(res.createdAt);
      $('.js-recent-user').html($copy);
    }, function(err) {
      console.error(err);
      window.alert('Something went wrong!');
    });
  });
})()

