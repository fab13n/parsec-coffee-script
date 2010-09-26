cs = require "./../src/cs"
gg = require "./../src/GrammarGenerator"

t = cs.parse """
compute 1*2+3/4
foo(x,y)
"""

print cs.toIndentedString t
