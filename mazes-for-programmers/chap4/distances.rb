class Distances
    def initialize(root)
        @root = root
        @cells = {}
        @cells[@root] = 0
    end

    def [](cell)
        @cells[cell]
    end

    def []=(cell, distance)
        @cells[cell] = distance
    end

    def cells
        @cells.keys
    end

    def path_to(goal)
        current = goal
        breadcrumbs = Distances.new(@root)
        breadcrumbs[current] = @cells[current]
        until current == @root
            current.links.each do |neighbour|
                if @cells[neighbour] < @cells[current]
                    breadcrumbs[neighbour] = @cells[neighbour]
                    current = neighbour
                end
            end
        end

        breadcrumbs
    end

    def max
        max_distance = 0
        max_cell = @root

        @cells.each do |cell, distance|
            if distance > max_distance
                max_cell = cell
                max_distance = distance
            end
        end

        [max_cell, max_distance]
    end

end

