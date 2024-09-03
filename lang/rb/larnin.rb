# running thru learnxiny, tryruby, & probabaly some others

#### LEARNXINY #####
# comments yas
=begin
apparently also comments
multiline style
but no one uses them
so whatevs
=end

# errythang is an obj

1.to_s # '1' (to string)

# operators are as expected:
# + - * / % and **
# bitwise: & | ^
# operators are actually just calling methods on objects
4.*(4) # 16, see?

true # truth
false # falsehood
nil # null, the only falsey val (besides `false`)
!nil == !false # because they're both true
!0 # false

4 == 1
4 != 8

# < > <= >=
# <=> ????? what????
# ! || && ; also `and` and `or` (lower precendence)

'foo'.class # str; `.class` is same as `typeof`?
1.class # fixnum

bar = 'thing' # assignment
"interpolation is only a #{bar} in double quotes"

# type coercion is manual!
'quux ' + 'baz' # 'quux baz'; note space.
'foo bar x' + 2 # error, but if we do
'foo bar x' + 2.to_s # it works! ('foo bar x2')
'quux ' << 'baz' # append (crap, it's the exact opposite of in bash xD)

puts 'foo' # returns 'foo\n'
print 'bar' # returns 'bar' (no '\n')

a = b = 'c' # multiple assignments are fine!

foo_bar_quux = 'baz' # use snake case

# symbols: immutable, reusable. used instead of strings sometimes
# for specific vals

:something.class # symbol
something_else = :something
something_else = 'foo' # false
something_else = :another_thing # false

example_array = [1, 2, 3] # same as in js
another_array = [4, false, 'stuff'] # valid

example_array[0] # 1
another_array.first # 4
another_array[9] # nil
# same as with maths, this is just methods on the array object
example_array.[] 0 # 1
array[-1] # last index
array.last # last index
array[3, 5] # returns five elements, starting at index 3
array.reverse! # reverses array
array[0..5] # destructures
array << 'foo' # array.push()
array.push('bar') # same thing!
array.include?('foo') # array.includes()

# hashes are like... uh, like hashes.
# like javascript's objects. just k/v pairs.
my_hash = { 'key' => 'value', 'key_two' => 'value_two' }
my_hash.keys # ['key', 'key_two']
my_hash['key_two'] # 'value_two'
my_hash['whatever'] # nil
# using symbols as keys in hashes:
symbol_hash = { something: 2, other_thing: false }
symbol_hash.key?(:something)
symbol_hash.value?(2) # those both return true

# arrays and hashes are enumerable
# they share methods like each, map, count, etc.

if true
  'stuff'
elsif false
  'if we need an \'else if\''
else
  'if we need this thing'
end

for something in 1..10
  puts "here's #{something}"
end
# loops are kinda not in fashion i guess in ruby?
# use each instead. pass it a 'block' (like, an anonymous function, i guess)
# so that same loop would be:
(1..10).each do |newthing|
  puts "here's #{newthing}"
end
# or, putting blocks in brackets, for why i don't know:
(1..10).each { |another| puts "here's #{another}" }

# .each works over data structures
arr.each do |el|
  puts "#{el}"
end
ha.each do |k, v|
  puts "#{k}:#{v}"
end

arrayyy.each_with_index do |element, index|
  puts "at index #{index}, we have #{element}"
end

counting = 1
while counting <= 10 do
  puts "we're now at numbah #{counting}"
  counting +=1
end

# map, reduce, inject, etc., are things
foo_array = [1,2,3,4,5,6,7,8,9,10]
bar_array = foo_array.map do |elementary|
  elementary * 2
end
puts foo_array # expected
puts bar_array # expected (foo_array, doubled)

# switch statements (case...when)
out_of_variable_names = 'why'

case out_of_variable_names
when 'what'
  puts 'hi'
when 'how'
  puts 'hello'
when 'who'
  puts 'sup?'
when 'what'
  puts 'blah'
when 'why'
  puts 'howdy'
when 'where'
  puts 'asdfghjkl;'
else
  puts 'how\'s it going?'
end

# if we're using integers, case can use ranges, like
# when 100..200 \n puts 'it\'s a pretty high number'
# or whatever

begin
  # stuff
  # i guess this stuff fails at life or whatever
  raise SomeError 'this is an error'
rescue SomeError => some_exception_var
  puts 'something happened', some_exception_var
rescue AnotherError => another_exception_var
  puts 'oh, dear...'
else
  puts 'i guess everything\'s just fine!'
ensure
  puts 'this will ALWAYS show up, yo!'
end

# THE IMPORTANT BIT, THE BIT I REALLY NEED TO KNOW:
# FUNCTIONS IN RUBY. FINALLY.

def foo(x)
  x * 2
end

foo(4) # 8
foo 8 # 16; because there's no ambiguity here
foo foo 10 # recursive (40)
# needs to have parens RIGHT after the call, if we're using parens (no space)
# take that, stupid space-lovers!

def bar(x, y)
  x * y
end

bar 2, 2 # 4
bar bar(2, 2), 2 # 8

# i'm not really sure what 'yield' does. i guess we'll come back to that.
# functions can take blocks as arguments, with the `&block` syntax.
def something(&stuff)
  stuff.call 'arg'
end

def whatever(*arr)
  arr.each { |ghjkl| puts things }
end
# that's called a 'splat' operator, apparently.
# it takes your list of arguments, and converts to an array

# destructuring:
def such_def
  ['wow', 'very', 'function']
end
doge, speak, lol = such_def
doge # wow
speak # very
lol # function

# if a method returns a boolean, it ends with a question mark
2.even? # true
# if a method is destructive, it ends with an exclamation
# methods might have both a ! and a non-! version
name = 'Zac Anger'
name.upcase # 'ZAC ANGER'
name # 'Zac Anger'
name.upcase! # 'ZAC ANGER'
name # 'ZAC ANGER'

# classes
class Me

  @@hair = 'reddish, i guess' # all instances now have this var

  def init(name, age = 26)
    # name will be an argument
    # age will too, but will default to 26 (like in es6(or is that 7?))
    @name = name
    @age = age
  end
  def name=(name)
    @name = name
  end
  # setter. and getter.
  def name
    @name
  end

  attr_accessor :name # i don't know what this means yet
  attr_reader :name
  attr_writer :name # srsly wtf is going on here

  def self.say(things)
    puts things
  end
  # 'self' is different bc only in class, not in instance

  def hair
    @@hair
  end
end

z = Me.new('zac anger')
g = Me.new('geordyn ader', 20)
z.name # 'zac anger'
z.age # 26
g.age # 20
Me.say('stuff') # 'stuff'

# scope:
$foo = 'this is global'
defined? $foo # 'global-variable'
@bar = 'this is not'
defined? @bar # 'instance-variable'
@@quux = 'this is class-scoped'
defined? @@quux # 'class variable'
Baz = 'this is a constant'
defined? Baz # 'constant'
# class variables are shared between the class and all
# that inherit from it

class You < Me # derived class
end
# now You have all the same methods as Me
# but You DON'T have the class-instance variables from Me

# modules! inclusion binds methods to _instances_ of a class
# extension binds methods to the actual class itself
module SomeMod
  def foo
    'foo'
  end
end

class Something
  include SomeMod
end

class Elsething
  extend SomeMod
end

Something.foo # nope
Something.new.foo # 'foo'
Elsething.foo # 'foo'
Elsething.new.foo # nope

module Example
  def self.included(base)
    base.extend(ClassMethods)
    base.send(:include, InstanceMethods)
  end

  module ClassMethods
    def bar
      'bar'
    end
  end

  module InstanceMethods
    def quux
      'quux'
    end
  end
end

class AnotherOne
  include Example
end

AnotherOne.bar # 'bar'
AnotherOne.quux # nope
AnotherOne.new.bar # nope
AnotherOne.new.quux # 'quux'
# i'm not entirely sure i understand that bit.
# 'callbacks are executed when including and extending a module'
# so says the thing. what is this actually showing me, here?

##### SOME BASICS FROM ANOTHER PLACE ######

'string'.reverse
'string'.length
'z' * 4 # 'zzzz'
# .to_s to string
# .to_i to int
# .to_a to arr
# .max on an array returns largest thing from there
[1,7,3].max # returns 7
[2,1,3].sort # returns [1,2,3]
# replacing things in things (was this for in a heredoc? i dunno?)
# var['i-dislike-this'] = 'it-should-be-this-instead'
# this replaces the first instance of 'i-dislike-this'
var.lines.to_a.reverse # turns a heredoc's lines into an array
# then reverses the lines, so it's like str.split('\n').reverse()!
# there's also a .bytes, .chars, and probably a lot of other methods.
# and a .join, of course.

# hashes and dictionaries -- same thing, different words, whatever.

# werkin' with the fs
Dir.entries '/path' # show all at that path. can take wild cards:
Dir['/path/to/stuff/*.js']
print File.read('/home/z/bin/js/fizzbuzz.js')
FileUtils.cp('/home/z/bin/js/fizzbuzz.js', '/home/z/toy-problems/fizzbuzz.js')

File.open('/path/to/file.js', 'a') do |f|
end
# opens that file in append mode, i think... what's the |f| block, exactly?

File.mtime('/file') # checks modification time
File.mtime('/file').hour # just the hour

