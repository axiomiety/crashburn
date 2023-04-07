require 'grid'
require 'binary_tree'

grid = BitfieldGrid.new(10,10)
BinaryTree.onBitGrid(grid)
puts grid
grid2 = Grid.new(10,10)
BinaryTree.on(grid2)
puts grid2