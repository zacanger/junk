angular.module('seatingApp').directive('dmTable', function () {
  return {
    scope: {
      table: '='
    },
    templateUrl: 'app/directives/tableTmpl.html',
    controller: function ($scope) {
      $scope.swap = function (oldIndex, newStudentNumber) {}(function () {
        $scope.tablePeople = []
        for (var i = 0; i < $scope.table.length; i++) {
          $scope.tablePeople.push({
            studentNumber: $scope.table[i],
            swapFn: $scope.swap
          })
        }
      }())
    }
  }
})
