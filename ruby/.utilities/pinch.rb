class Pinch
  def initialize(obj)
    @object = obj
  end

  ##
  # get only the methods defined on the class object itself
  def unique_methods_for(klass)
    ancestor_methods = klass.ancestors.drop(1).flat_map(&:methods)
    klass.methods - ancestor_methods
  end

  def examine
    klass = @object.is_a?(Class) ? @object : @object.class
    ancestors = klass.ancestors
    prefix = "PINCH:"

    puts "#{prefix} #{@object} is a #{klass}"

    ancestors.each do |kls|
      puts "#{prefix} #{kls}: #{kls.methods.count} methods total"
      puts "#{prefix} #{unique_methods_for(kls).inspect}"
    end; nil
  end

  private
end

def pinch(obj)
  Pinch.new(obj).examine
end
