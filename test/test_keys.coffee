Array.prototype.toString = -> '[ ' + (@join ', ') + ' ]'
Function.prototype.toString = -> "<FUNCTION>"

if false
    Object.prototype.toString = ->
        '{' + (("#{k}: #{v}" for k, v of @).join ', ') + '}'

{ print, error, inspect } = require 'util'

gg = require "../src/GrammarGenerator"
cs = require "../src/CoffeeScriptParser"

kwlist = (set) ->
    ("'#{k.replace /^keyword\-/, '!'}'" for k of set).join ', '

orderedNames = (n for n of cs).sort()

for name, parser of cs
    continue unless parser instanceof gg.Parser
    print " * Force reindexing of #{name}\n"
    parser.reindex()

for name in orderedNames
    parser = cs[name]
    continue unless parser instanceof gg.Parser
    if parser.catcodes
        print "cs.#{name}\t--has-catcodes-->\t#{if parser.epsilon then 'epsilon + ' else ''}#{parser.catcodes2string()}\n"
        if parser instanceof gg.Expr
            if parser.primary
                print "\t expr primary keys: #{parser.primary.catcodes2string()}\n"
            else
                print "\t no primary parser\n"
            for setname in ['prefix', 'suffix']
                set = parser[setname]
                print "\t expr #{setname} catcodes: #{set.catcodes2string()}\n"
    else print "cs.#{name} has no key\n"

print '\n'

for name, parser of cs
    unless parser not instanceof gg.Parser or parser.catcodes
        print "(cs.#{name}: no catcode)\n"

print '\n'

for name, parser of cs
    unless  parser instanceof gg.Parser
        print "(cs.#{name}: not a parser)\n"

links = { }
for name in orderedNames
    p = cs[name]
    continue unless  p instanceof gg.Parser
    for q in p.listeners
        links[q.toString()+"\t--listens-to-->\t"+p.toString()]=true

print "\ndependencies:\n"
print (k for k of links).sort().join '\n'

