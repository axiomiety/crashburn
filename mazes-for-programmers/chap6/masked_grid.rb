require 'grid'

class MaskedGrid < Grid
    attr_reader :mask

    def initialize(mask)
        @mask = mask
        super(@mask.rows, @mask.columns)
    end

    def prepare_grid
        Array.new(rows) do |row|
            Array.new(columns) do |column|
                Cell.new(row, column) if @mask[row,column]
            end
        end
    end

    def random_cell
        row, col = @mask.random_location
        self[row,col]
    end

    def size
        @mask.count
    end
end
