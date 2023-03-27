class BinaryTree
    def self.on(grid)
        grid.each_cell do |cell|
            neighbors = []

            neighbors << cell.north if cell.north?
            neighbors << cell.east if cell.east?

            neighbor = neighbors.sample
            cell.link(neighbor) if neighbor
        end
    end

end


class BinaryTreeWithBias
    @@biases = [:north, :east]
    @@relative_weights = {:north => 1, :south => 1, :east => 1, :west => 1}
    def self.on(grid)
        grid.each_cell do |cell|
            neighbors = {}
            @@biases.each do |bias|
                neighbors[bias] = cell.public_send(bias) if defined? cell.public_send(bias)
            end

            # do a weighted sample
            # total_weights = 
            neighbor = neighbors[neighbors.keys.sample]
            cell.link(neighbor) if neighbor
        end
    end

end

class SouthWestBinaryTree < BinaryTreeWithBias
    @@biases = [:south, :west]
end