# https://github.com/jamesconant/bstree
require 'bstree'
root = Bstree::Node.new(5)
root.insert(2)
root.insert(7)
root.search(3)
# nil