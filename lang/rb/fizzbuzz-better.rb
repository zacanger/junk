#!/usr/bin/env ruby

1.upto(100) do |i|
  fizz = (i % 3 == 0)
  buzz = (i % 5 == 0)
  puts case
    when fizz && buzz then 'FizzBuzz'
    when fizz then 'Fizz'
    when buzz then 'Buzz'
    else i
  end
end

