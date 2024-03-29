require 'chunky_png'

class Mask
    attr_reader :rows, :columns

    def self.from_png(file)
        image = ChunkyPNG::Image::from_file(file)
        mask = Mask.new(image.height, image.width)
        mask.rows.times do |row|
            mask.columns.times do |col|
                if image[col, row] == ChunkyPNG::Color::BLACK
                    mask[row, col] = false
                else
                    mask[row, col] = true
                end
            end
        end
        mask
    end

    def self.from_txt(file)
        lines = File.readlines(file).map { |line| line.strip }
        lines.op while lines.last.length < 1
        rows = lines.length
        columns = lines.first.length
        mask = Mask.new(rows, columns)

        mask.rows.times do |row|
            mask.columns.times do |col|
                if lines[row][col] == "X"
                    mask[row, col] = false
                else
                    mask[row,col] = true
                end

            end
        end
        mask
    end

    def initialize(rows, columns)
        @rows, @columns = rows, columns
        @bits = Array.new(@rows) { Array.new(@columns, true)}
    end

    def [](row, column)
        if row.between?(0, @rows-1) && column.between?(0, @columns-1)
            @bits[row][column]
        else
            false
        end
    end

    def []=(row, column, is_on)
        @bits[row][column] = is_on
    end

    def count
        count = 0
        @rows.times do |row|
            @columns.times do |col|
                count += 1 if @bits[row][col]
            end
        end
        
        count
    end

    def random_location
        loop do
            row = rand(@rows)
            col = rand(@columns)
            return [row,col] if @bits[row][col]
        end
    end
end
