require 'grid'
require 'binary_tree'

grid = Grid.new(4,4)
BinaryTree.on(grid)
puts grid

deadends = grid.deadends
puts "#{deadends.count} dead-ends"