firebase with $firebaseObject (or $firebaseArray) and $bindTo just makes all
the __MAGIC__ (that we poor slobs are are too dumb to even vaguely comprehend)
even MORE magical; with angularfire and firebase and angular doing everything
for you, we now have, basically, a subpar meteor-like 3-way-binding.

<ng-change="$scope.save(thingy)"> will propogate back to firebase
(or <ng-change="thingy(save)"> // $scope.sav(thingy) if we're doing things
modularly).

ng-model-options="{debounce: {'default': 5000}}" // debounces. to 5000 ms. wowe.

