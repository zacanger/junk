directives are different than controllers and services. directives ALWAYS return an object. when you write a
ctrl or svc you're giving some definitions to something that already exists. with directives you're closer
to writing routing. you're writing a set of instructions. they return an object. objects get, obviously,
key-value pairs.

```javascript
.directive('myDirective', function(){
	return {
		template: 'Key: Value, key: value';
	}
})
```

directive names MUST be camelCase. pascal case will not be recognized. nor snake case?

i mean, really, it's all made a lot more complicated than it needs to be. we're starting out basically
just making our own little bits. it makes sense. if it works as a template file, or as just regular html,
it'll work as a custom directive. just more reusable as a directive.

we could have a directive with a TYPE. imagine a function that returns a customer with an attribute that's
.type... so we'd have a directive called myCustomer, type="name", then another, type="smoking habits".
paramaters here are always (element, attribute).

Restrict as a property: E means element (so you'd do <myDirective> instead of <div myDirective>
A is attribute, so it'd be <div myDirective>
can do BOTH so it'd work for either. (restrict: EA)
C is for class, but let's not do that! That seems like a really not okay and very bad idea.
This is basically for semantic enforcement. Element would be good for a custom block, eg
<myDirective>a whole bunch of crap just about this one</myDirective>
<mySECONDdirective>another whole bunch</mySECONDdirective> etc
A would be good for tacking things onto elements that would likely be there anyway, it seems.
If you need to take in parameters, A would be better.

custom filters like this!

<input model="whatcha">
<span custom="format">
function link(scope, element, attributes){
var whatever
stuffings
}
return { link: link; }
scp.$watch('format', function(value){
format=value;
updateTime();
});


link ALWAYS takes in at leaste those three, and alkways in that order!

inside a directive, feel free to do some jquery-lite.
if you want to use, say, d3, or three.js, you can go ahead and inject that into angular
and use it. (we're talking about other DOM manupilation, not utilities like lodash).

inside a link whatever, that scp refers to its parent's scope, it shares parent's scope
by default. eg if custom up there is directly below mainCtrl, scp means mainCtrl's $scope.

underneath .directive, the scope: { stuff } actually means, like, scope INSTRUCTIONS or
somesuch.


SCOPE
```javascript
scope: {
 string: '@',
 link: '=',
 func: '&'
}
```

The properties on the scope object represent the attributes on the directive in the html. Our example scope object here would look something like this in the html.
`<example-directive string="a string" link="user" func="updateUser()"></example-directive>`
The hard part here is the `@`, `=`, and `&`. They each have very important and distinct meanings.
- `@` says take in my attribute value as a string.
- `=` says take in my attribute value as a two-way bound variable from the parent scope.
- `&` says take in my attribute value as a reference to a function on the parent scope.



