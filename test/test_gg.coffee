gg  = require './mcs/GrammarGenerator'
lex = require './mcs/Lexer'

# Need to see boundaries of arrays when printed
Array.prototype.toString = -> '[ ' + (@join ', ') + ' ]'

indented = (p) ->
    (gg.sequence gg.indent, p, gg.dedent).setBuilder 1

optind = (p) -> gg.choice (indented p), p

g = { }
g.keywords  = new lex.Keywords 'let', 'call'
g.assign    = gg.sequence 'let', gg.id, '=', gg.number
g.call      = gg.sequence 'call', gg.id, optind (gg.list gg.id, (gg.choice ','))
g.statement = gg.choice g.assign, g.call
g.statlist  = gg.list g.statement, gg.choice gg.newline, ';'
g.chunk     = (gg.sequence gg.indent, g.statlist, gg.dedent).setBuilder 1
g.cmp       = indented gg.choice '<', '=', '>'

g.expr = gg.expr gg.number

g.expr.addInfix  { parser:'+', prec:50, builder: (a,_,b) -> "(#{a} + #{b})" }
g.expr.addInfix  { parser:'*', prec:60, builder: (a,_,b) -> "(#{a} * #{b})" }
g.expr.addInfix  { parser:'^', prec:70, assoc:'right', builder: (a,_,b) -> "(#{a} ^ #{b})" }
g.expr.addInfix  { parser:'$', prec:55, assoc:'flat', builder: (a) -> "$(#{a.join ', '})" }
g.expr.addPrefix { parser:'-', prec:80, builder: (_,a) -> "(- #{a})" }


run = (p, src) ->
    l=new lex.Lexer src, g.keywords
    r=p.parse l
    if r then print 'parsed: ', r, '\n'
    else print "cannot parse\n"

print 'initialized\n\n'

if false
    run g.chunk, '''
    let x1 = 123
    let x2=456
    call foo bar, bar
    let y=789; let z=345'''

if false
    run g.cmp, '>'

if true
    p = indented g.expr
    #run p, '-2+3'  # precedence on infix ops
    #run p, '1+2+3' # left assoc
    #run p, '1^2^3' # right assoc
    run p, '1$2$3+4$5$6' # flat op