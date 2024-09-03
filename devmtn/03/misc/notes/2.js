// so, jeremy's super-boiled-down 10-second run-through of the MVC pattern definitely clarified the actual role of the controller. i mean, i kind of understood that it's basically binding models and views, but never really understood it as the actual middleman until just now. that is awfully sensible.
//
// implied, but not stated explicitly: react is less of a bottleneck as a view than angular is.
//
// something to keep in mind: `$scope` is straight up two-way binding, not sortof or sometimes as was implied in the article.
//
// oh, `directive` is basically just silly slang for attributes.
//
// mismatched bits and bobs? not with angular, it'll just boink up some fresh new dom nodes for your loose-ends
//
// $timeout basically same syntax as setTimeout, except
```
$timeout(function(){
	$scope.thing = "that";
		}, 10000 );
```

EVEYTHING that goes IN the html that angular will use is INSIDE of ng-controller, attached to its $scope

so for the  HTML to even give a crap about what's going on in your javascript, it's gotta be in a $scope. everything that you want angular to actually act onhad better be {{}} in the html and $scope in your js

as jeremy explained it, your js as it sits in your files is ALSO basically kind of a starting point, because (and see 1.md for more notes on triggering events), what angular does is (he said scan for  but i understood it as wait for triggers from) basically an excuse to go changing things. so your objects, variables, whatever are OH MY GOD NOW I KNOW WHAT IMMUTABLE JAVASCRIPT IS A BIG DEAL OKAY (no, not because of angular, but because it just hit me how how loose EVERYTHING can really be in javascript... wow, no wonder js get so much hate. it probably deserves it.)

ng-repeat makes a miniature scope for the foo part of `ng-repeat="foo in bar"` -- underneath, i suppose. that's super slick.

<input ng-model="someFilter"> // filter is a keyword to angular, it knows to look for a filter as defined by itself
<div ng-repeat="some in such | filter: someFilter">
{{some.stuff}} {{some.things}} // okay, and i suppose, it looks like filter is just a builtin that does just that one
{{some.stuff}} {{some.things}} // exact thing... it only shows the thing that you're inputting/providing as a filter
{{some.stuff}} {{some.things}} // BUT that does not necessarily have to mean actual filtered string fragments.
{{some.stuff}} {{some.things}} // THIS MAKES LIFE EASY.
</div>
<div ng-repeat="some in such | filter: { name: someFilter location { name: locationNameFilter } }">
// so basically we're interactively filtering here, but also filtering by the type of whatchamacallits,
// filtering the objects with a nesting (so, narrowing) way. we could that we need to pass an object,
// and that object ought to have a certain .property, and that key's value should PROBABLY have a capital A
// or else, y'know, hey angular, throw that crap out.
// that, right there, could be a simple (though obviously not exactly performant...) way to work on that
// little knowledge management problem we have.
// <div ng-init="whatisUPYO = true">YO</div>
// <div ng-init="heywhatever = false">YO</div>
// <div ng-show="heywhatever">YOU CAN'T SEE ME</div>
// <div ng-show="whatisUPYO">BUT YOU CAN SEE ME</div>
// within the ng-thingy"IN HERE THIS PLACE THIS BIT RIGHT HERE" you can
// basically work with any executable javascript code you'd like...
// though that would be an awfully bad idea, i suppose.
//
// ... we have ng-hide also, which is just ng-show-" = false "
// so you're basically only hiding if (true/truthy). negates the need for
// double negatives, or some such.
// they're only really working on css `display: none;`
// ng-if will actually keep something off the dom if it fails, so
// HOLY CRAP ANGULAR HAS A BUILT IN LINK FILTER. FILTER IS NOT JUST FILTER.
// this means you can probably pipe to a whole ughing LOAD of things, and
// seeing as scripting languages basically mostly exist to abstract themselves
// more and more every year until they grow actual intelligences...
// THIS MEANS THIS IS BASICALLY LIKE POSIX PIPES. WHICH MEANS I CAN, WITH A BIT OF WORK
// AND A LOT OF SYNTAX HELP AND PROBABLY ALL SORTS OF FAILURES, ESCAPING, SANITIZING,
// AND TESTING... basically it could become fairly easy to port posix utilities to
// angular. and since that's written in javascript... that could be extracted and
// maybe combined and turned into its own library for porting posix-compliant shell
// scripts (or, theoretically, any, i guess...) to... the... browser.
// oh gosh. this clarified some things, for sure.
// though i STILL DON'T KNOW WHY WE NEED TO DO TEMPLATING WITH MORE PUNCUATION
// RATHER THAN LESS
// JADE MAKES THINGS EASIER
// SO DOES STYLUS
// SO WHAT THE HELL WHY CAN'T JAVASCRIPT TEMPLATING FOR JAVASCRIPT ACTUALLY BE GOOD



