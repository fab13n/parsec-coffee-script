Array.prototype.toString = -> '[ ' + (@join ', ') + ' ]'
#Object.prototype.toString = ->
#    '{' + (("#{k}: #{v}" for k, v of @).join ', ') + '}'

cs = require "../src/CoffeeScriptParser"
gg = require "../src/GrammarGenerator"

print "\n\n--- Keys dump ---\n"

kwlist = (set) ->
    ("'#{k.replace /^keyword\-/, '!'}'" for k of set).join ', '

for name, parser of cs
    if parser.keys
        print "cs.#{name}\t--has-keys-->\t#{kwlist parser.keys}\n"
        if parser instanceof gg.Expr
            for setname in ['prefix', 'infix', 'suffix']
                set = parser[setname]
                print "\t expr #{setname} keys: #{kwlist set}\n"

print '\n'

for name, parser of cs
    unless parser not instanceof gg.Parser or parser.keys
        print "(cs.#{name}: no keys)\n"

print '\n'

for name, parser of cs
    unless  parser instanceof gg.Parser
        print "(cs.#{name}: not a parser)\n"

links = { }
for name, p of cs
    continue unless  p instanceof gg.Parser
    for q in p.listeners
        links[q.toString()+"\t--listens-to-->\t"+p.toString()]=true

print "\ndependencies:\n"
print (k for k of links).sort().join '\n'

