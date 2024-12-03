class AoC
  def self.day3a(input)
    ops = input.scan(/mul\(\d+,\d+\)/)
    res = ops.map do |line|
        vals = line.match(/mul\((\d+),(\d+)\)/)
        vals[1].to_i * vals[2].to_i if vals
      end.compact
    puts "Part 1: " + res.sum.to_s
  end

  def self.day3b(input)
    ops = input.scan(/mul\(\d+,\d+\)|don't\(\)|do\(\)/)
    onoff = 1
    tot = 0
    res = ops.map do |line|
        case line
        when "do()"
            onoff = 1
        when "don't()"
            onoff = 0
        else 
            vals = line.match(/mul\((\d+),(\d+)\)/)
            tot += onoff*(vals[1].to_i * vals[2].to_i) if vals
        end
    end
    puts "Part 2: " + tot.to_s
  end
end



input = File.read('../R/inst/input03.txt')

puts AoC.day3a(input)
puts AoC.day3b(input)