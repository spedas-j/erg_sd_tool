radardat_fpath = "/home/ergsc-cdf/work/cdf/superdarn/sddata/software/current/tables/superdarn/radar.dat"

open(radardat_fpath) do |fp|
  
  fp.each_line do |line|
    line.chomp!
    next if line.length < 10
    words = line.split('"')
    radarno = (words[0].split)[0]
    puts radarno+" "+words[7]
  end

  
  
end

