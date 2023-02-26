require 'grid'
require 'sidewinder'

grid = Grid.new(6,6)
Sidewinder.on(grid)

puts grid
img = grid.to_png
img.save "maze.png"