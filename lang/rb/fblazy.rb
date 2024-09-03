#!/usr/bin/env ruby

fizz = ["","","Fizz"].lazy.cycle
buzz = ["","","","","Buzz"].lazy.cycle
nums = (1..Float::INFINITY).lazy

fizzbuzz = nums.zip(fizz,buzz).map do |n,f,b|
  (f.empty? && b.empty?) ? n.to_s : f + b
end

puts fizzbuzz.take(100).to_a

