require 'distance_grid'
require 'binary_tree'

grid = DistanceGrid.new(5,5)
BinaryTree.on(grid)

start = grid[0,0]

distances = start.distances
new_start, distance = distances.max

new_distances = new_start.max
goal, distance = new_distances.max

goal.distance = new_distances.path_to(goal)
puts grid