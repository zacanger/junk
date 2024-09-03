#!/usr/bin/env ruby

def fizzbuzz(num)
  result = ''
  result += 'Fizz' if num % 3 == 0
  result += "Buzz" if num % 5 == 0
  puts(result.empty? ? num : result)
end

(1..100).each{|x|
  fizzbuzz(x)
}

