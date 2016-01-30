# print the string 'Hello, world'
puts 'Hello, world'

# find the index of the word Ruby in 'Hello, Ruby'
puts 'Hello, Ruby'.index('Ruby')

# print your name 10 times
10.times { puts 'bob' }

# print the string 'this is sentence numbre 1' where 1 changes from 1 to 10
(1..10).each {|i| puts "This is sentence number #{i}"}

# run a ruby program from a file
# ruby <prog.rb>

# bonus! write a program that picks a random number and lets the user guess - telling whether too high or too low
def play
  number = rand(10)
  loop do
    guess = gets.chomp.to_i # don't want the new line, need to convert to integer
    break if guess == number
    guess < number ? (puts 'too low') : (puts 'too high')
  end
  puts 'You guessed it!'
end

play
