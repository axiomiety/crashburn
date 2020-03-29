# access files with/without code blocks


# translate hash to array, can you translate arrays to hashes
h = {:foo =>1, :bar => 3, :foobar => 7}
puts h.to_a
## to convert to a hash, 2.1 has a to_h method or use:
Hash[*h.to_a.flatten]
## TODO: figure out why you can call Hash with [ ] straight off the bat!

# iterate through a hash
h.map{|k,v| puts "key:#{k} value:#{v}"}

# use Ruby arrays as stacks - what other data structures can arrays be used for?
## stacks and queues!
a = [3,2,10]
a.unshift(1) # enqueue from the left
a.pop # remove from the right

# print contents of an array of 16 numbers, 4 at a time, using each. again using each_slice
aa = (1..16).to_a
## using a counter
idx = 0
aa.each do
  elem = aa[idx]
  print (idx % 4 == 0) ? "#{elem}\n" : "#{elem},"
  idx += 1
end
## using an integrated counter
aa.each_with_index{|idx, elem| print (idx % 4 == 0) ? "#{elem}\n" : "#{elem},"}
aa.each_slice(4) { |s| puts s.join(' ')}

# Tree class - init accept a nested structure

class Tree
  attr_accessor :children, :node_name

  F = {'grandpa' => {
                  'dad' => {
                      'child1' => {},
                      'child2' => {},
                  },
                  'uncle' => {
                    'child3' => {},
                    'child4' => {},
                  }
                }
              }

  def initialize(tr)
    # base case is {'root' => {}}
    @children = []
    @node_name = tr.keys.pop
    tr[@node_name].each_pair do |k,v|
      @children.push( Tree.new( {k=>v} ) )
    end
  end

  # old impl
  def initialize_old(name, children=[])
    @children = children
    @node_name = name
  end

  def visit_all(&block)
    visit &block
    children.each{ |child| child.visit_all &block }
  end

  def visit(&block)
    block.call self
  end
end

#t = Tree.new('Ruby', [Tree.new('Reia'), Tree.new('MacRuby')])
#t.visit_all{ |node| puts node.node_name }

# we want to move to
family_tree = {'grandpa' => {
                  'dad' => {
                      'child1' => {},
                      'child2' => {},
                  },
                  'uncle' => {
                    'child3' => {},
                    'child4' => {},
                  }
                }
              }
#family_tree = {'root'=>{'child'=>{}}}
ft = Tree.new(family_tree)
ft.visit_all{ |node| puts node.node_name }

def ruby_grep
  # simple grep that print lines of a file having any occurences of a phrase anywhere in that line
  puts 'word to search for:'
  word = gets.chomp
  puts "searching for #{word}"
  IO.foreach('/etc/dictionaries-common/words') do |line|
    puts "found #{word} in <#{line}>" if line.include?(word)
  end
end
