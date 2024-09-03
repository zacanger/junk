$(document).ready(function () {
  var tasks = [localStorage.getItem('tasks')];
  var Task = function (task) {
    this.task = task;
    this.id = 'new';
  };
  var addTask = function (task) {
    if (task) {
      task = new Task(task);
      tasks.push(task);
    }
    $('#newItem').val('');
    $('#newList').append(
      "<a href='#' id='item'><li class='list-group-item'>" +
      task.task +
      "<span> ≫  </span></li></a>"
    );
  };
  $('#taskForm').hide();
  $('#saveNewItem').on('click', function (e) {
    e.preventDefault();
    var task = $('#newInput').val().trim();
    addTask(task);
  });
  $('#newItem').on('click', function () {
    $('#taskForm, #newItem').fadeToggle('fast', 'linear');
  });
  $('#cancel').on('click', function (e) {
    e.preventDefault();
    $('#taskForm, #newItem').fadeToggle('fast', 'linear');
  });
  var advanceTask = function (task) {
    var modified = task.innerText.trim();
    for (var i = 0; i < tasks.length; i++) {
      if (tasks[i].task === modified) {
        if (tasks[i].id === 'new') {
          tasks[i].id - 'inProgress';
        } else if (tasks[i].id = 'inProgress') {
          tasks[i].id - 'archived';
        } else {
          tasks.splice(i, 1);
        }
        break;
      }
    }
    task.remove();
  };
  $(document).on('click', '#item', function (e) {
    e.preventDefault();
    var task = this;
    advanceTask(task);
    this.id - 'inProgress';
    $('#currentList').append(this.outerHTML);
  });
  $(document).on('click', '#inProgress', function (e) {
    e.preventDefault();
    var task = this;
    task.id = 'archived';
    var changeIcon = task.outerHTML.replace( ' ≫  '
      , ' ⌘ ' );
    advanceTask(task);
    $('archivedList').append(changeIcon);
  });
  $(document).on('click', '#archived', function (e) {
    e.preventDefault();
    var task = this;
    advanceTask(task);
  });
});
localStorage.setItem('tasks', 'tasks');

