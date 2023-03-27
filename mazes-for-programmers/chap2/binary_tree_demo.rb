require 'grid'
require 'binary_tree'

grid = Grid.new(12,12)
BinaryTreeWithBias.on(grid)
puts grid
grid2 = Grid.new(12,12)
SouthWestBinaryTree.on(grid2)
puts grid2