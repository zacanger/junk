this.whatever makes 'whatever' (in a service) visible to the controller.

it's a good idea to go ahead and be like


var hello = 'oi!';

this.returnedInfo = function(){
  return hello;
};


that way... data is made, protected, changed, broken, etc., all in one place. just accessed through that nice little function there.
i am so confused about bananas


