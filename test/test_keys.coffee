Array.prototype.toString = -> '[ ' + (@join ', ') + ' ]'
Function.prototype.toString = -> "<FUNCTION>"

if false
    Object.prototype.toString = ->
        '{' + (("#{k}: #{v}" for k, v of @).join ', ') + '}'

cs = require "../src/CoffeeScriptParser"
gg = require "../src/GrammarGenerator"

kwlist = (set) ->
    ("'#{k.replace /^keyword\-/, '!'}'" for k of set).join ', '


for name, parser of cs
    continue unless parser instanceof gg.Parser
    parser.reindex()

print "\n\n--- Keys dump ---\n"

for name, parser of cs
    continue unless parser instanceof gg.Parser
    if parser.keys
        print "cs.#{name}\t--has-keys-->\t#{kwlist parser.keys}\n"
        if parser instanceof gg.Expr
            if parser.primary
                print "\t expr primary keys: #{kwlist parser.primary.keys}\n"
            else
                print "\t no primary parser\n"
            for setname in ['prefix', 'infix', 'suffix']
                set = parser[setname]
                print "\t expr #{setname} keys: #{kwlist set}\n"
    else print "cs.#{name} has no key\n"

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

print '\n\ninfix:\n'

print cs.expr.infix