class BinaryTree
    def self.onBitGrid(grid)
    end
    def self.on(grid)
        grid.each_cell do |cell|
            neighbors = []

            neighbors << cell.north if cell.north
            neighbors << cell.east if cell.east

            neighbor = neighbors.sample
            cell.link(neighbor) if neighbor
        end
    end
end

class BinaryTreeWithBias
    attr_reader :biases
    def initialize(*biases)
        @biases = biases.to_a
    end

    def on(grid)
        grid.each_cell do |cell|
            neighbors = []
            @biases.each do |bias|
                neighbors << cell.public_send(bias) if defined? cell.public_send(bias)
            end

            neighbor = neighbors.sample
            cell.link(neighbor) if neighbor
        end
    end
end

class BinaryTreeWithBiasAndWeights 
    attr_reader :weights, :biases
    def initialize(direction_to_weights)
        @weights = direction_to_weights
        @biases = @weights.keys
    end
    def on(grid)
        grid.each_cell do |cell|
            neighbors = {}
            @biases.each do |bias|
                neighbors[bias] = cell.public_send(bias) if defined? cell.public_send(bias)
            end

            # do a weighted sample
            running_sum = 0
            biases_boundaries = []
            @biases.each do |bias|
                running_sum += @weights[bias]
                biases_boundaries << running_sum
            end

            total_weights = @biases.map { |bias| @weights[bias]}.sum()
            selection = rand(total_weights+1) # because we don't include the max
            neighbor = neighbors[@biases[biases_boundaries.bsearch_index{|elem| elem >= selection}]]
            cell.link(neighbor) if neighbor
        end
    end
end

class SouthWestBinaryTree < BinaryTreeWithBias
    @@biases = [:south, :west]
end