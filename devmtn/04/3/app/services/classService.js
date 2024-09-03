angular.module('seatingApp').service('classService', function () {
  var classroom = []
  this.getClass = function () {
    return classroom
  }
  this.addClass = function (classObj) {
    var allTables = []
    var students = []
    for (var i = 0; i < classObj.studentCount; i++) {
      students.push(i + 1)
    }
    for (var i = 0; i < classObj.tableCount; i++) {
      var newTable = []
      allTables.push(newTable)
    }
    var currentTable = 0
    for (var i = students.length - 1; i >= 0; i--) {
      var studentIdx = Math.floor(Math.random() * (i - 0))
      allTables[currentTable].push(students[studentIdx])
      currentTable++
      students.splice(studentIdx, 1)
      if (currentTable >= allTables.length) {
        currentTable = 0
      }
    }
    classObj.seatingAssignments = allTables
    classroom.push(classObj)
  }
  this.addClass({
    name: 'Ninja',
    tableCount: 8,
    tableCapacity: 4,
    studentCount: 20
  })
})
