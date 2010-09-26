{ Lexer } = require('./../src/Lexer')

# Testing code #

test1 =
'''
#1 "foo #{bar{}}WWW"
2 "str0 #{tok1 "#{tok2}" tok3} nest0"
3 ab +  cd++
4 ef # hi
    5 'gh'
    6 ij
        7 "kl" ""
    8 mn "t'r''i'p''l'e"
        9 op
10 qr
    11 st
'''

print(test1,'\n')

ml = new Lexer test1

ml.keywords.add('++', '--', 'bar')

tokens = ml.tokenize()
for t in tokens
    if typeof t.v == 'string' and t.v != (x=test1[t.i...t.i+t.v.length])
        print "WARNING: offset problem with #{t} / #{x}\n"
    if t.t == 'indent' or t.t == 'indent' or t.t == 'newline'
        if ml.src[t.i] != '\n'
            print "WARNING: offset problem with #{t}\n"

for t in tokens
    print "#{ml.offsetToLine(t.i)}\t#{t}\n"

print tokens.join '\t'
