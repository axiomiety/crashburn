# modify the csv app - add an `each` method that returns a CsvRow, and use `method_missing on `CsvRow` to return the value given a heading
# one, two
# lieons, tigers
# csv = RubyCsv.new
# csv.each {|row| puts row.one}


class CsvRow
  attr_accessor :headers, :row

  def initialize(headers, row)
    @headers = headers
    @row = row
  end

  def method_missing(method, *args, &block)
    col_idx = @headers.index(method.to_s) #`method` is of type Symbol
    if col_idx != nil
      @row[col_idx]
    else
      # we should raise some kind of exception
      puts 'woops...'
    end
  end
end

class ActsAsCsv
  def self.acts_as_csv
    define_method 'read' do
      file = File.new(self.class.to_s.downcase + '.txt')
      @headers = file.gets.chomp.split(',')
      file.each do |row|
        @result << row.chomp.split(',')
      end
    end

    define_method 'headers' do
      @headers
    end

    define_method 'csv_contents' do
      @result
    end

    define_method 'initialize' do
      @result = []
      read
    end


  end
end

class RubyCsv < ActsAsCsv
  acts_as_csv
    def each
      @result.each do |row|
        r = CsvRow.new(@headers,row)
        yield r
      end
    end
end

