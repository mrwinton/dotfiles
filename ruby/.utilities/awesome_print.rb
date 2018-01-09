begin
  puts 'Loading awesome_print'
  require 'awesome_print'
  AwesomePrint.pry!
rescue LoadError => err
  warn "Unable to load awesome_print #{err}"
end
