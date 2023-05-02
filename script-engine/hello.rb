#!/usr/bin/env ruby
 
print "What is your name? "
name = gets
name ||= ''        # set to empty string if nil
name.chomp!        # remove trailing newline 
puts "Hello #{name}!"
return name