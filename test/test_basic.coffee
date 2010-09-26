# implement a super-basic language to test gg
#

cs = require "./../src/cs"
gg = require "./../src/GrammarGenerator"

Array.prototype.toString = -> '[ ' + (@join ', ') + ' ]'

# TODO: indented toString
tree = (x...) -> new Tree x...
class Tree
    constructor: (@tag, @children...) ->
    toString: -> "`#{@tag}(#{@children.join ', '})"

toIndentedString = (x) ->
    rec = (x, b, i) ->
        if x instanceof Tree
            b.push x.tag
            nChildren = x.children.length
            if nChildren==0
                return b
            else
                b.push "( "
                for child, n in x.children
                    rec(child, b, i+"  ")
                    b.push (if n<nChildren-1 then ",\n  "+i else " )")
        else if x instanceof Array
            b.push "[ "
            nChildren = x.length
            for child, n in x
                rec(child, b, i+"  ")
                b.push(if n<nChildren-1 then ",\n  "+i else  " ]")
        else b.push x.toString()
    b = [ ]
    rec(x, b, '')
    return b.join("")


basic = { }

basic.keywords = new lex.Keywords "true", "false", "for", "to", "do", "if", "then", "else"

basic.primary = gg.choice(
    (gg.wrap gg.number).setBuilder (x) -> tree 'num', x
    (gg.wrap gg.id)    .setBuilder (x) -> tree 'id', x
    #gg.string, not implemented
    "true",
    "false"
)



basic.expr = e = gg.expr(basic.primary)

basic.parenParams = gg.sequence(gg.noSpace, '(', gg.list(e, ','), ')').setBuilder 2

opbuild = (op) -> (a, _, b) -> tree "op", op, a, b
e.addInfix  {parser:'=', prec:20, assoc:'right', builder: (a,_,b)-> tree 'set', a, b}
e.addInfix  {parser:'+', prec:50, assoc:'left',  builder: opbuild '+'}
e.addInfix  {parser:'-', prec:50, assoc:'left',  builder: opbuild '-'}
e.addInfix  {parser:'*', prec:60, assoc:'left',  builder: opbuild '*'}
e.addInfix  {parser:'/', prec:60, assoc:'left',  builder: opbuild '/'}
e.addPrefix {parser:'-', prec:70, builder: (_,a) -> tree "op", '-', a}
e.addSuffix {parser: gg.list(e, ','), prec:100, builder: (x...) -> tree "call", x...}
e.addSuffix {parser: basic.parenParams, prec:100, builder: (x...) -> tree "call", x...}

basic.primary.add(gg.sequence("(", basic.expr, ")").setBuilder 1)

basic.statement = gg.choice(
    basic.expr
)

basic.block = gg.sequence(
    gg.indent,
    gg.list(basic.statement, gg.choice(";", gg.newline)),
    gg.dedent
).setBuilder (x) -> print "block #{x[1]}\n"; x[1]

basic.statLine = gg.list(basic.statement, ";")

basic.if = gg.sequence(
    "if", basic.expr,
    gg.choice(
        gg.sequence("then", basic.statLine).setBuilder(1),
        basic.block
    ),
    gg.if(
        gg.choice("else", [gg.newline, "else"]),
        gg.choice(basic.statLine, basic.block),
        [])
).setBuilder (x) -> tree "if", x[1], x[2], x[3]

basic.for = gg.sequence(
    "for", gg.id, "=", basic.expr, "to", basic.expr,
    gg.choice(
        gg.sequence("do", basic.statLine).setBuilder(1),
        basic.block
    ),
).setBuilder (x) -> tree "for", x[1], x[3], x[5], x[6]

basic.statement.add basic.if, basic.block, basic.for

basic.parse = (src) -> basic.block.parse(new lex.Lexer(src, basic.keywords))

#print "\n"+ basic.parse "foo 1, bar 1+2*3"
#print "\n"+ basic.parse "foo 1, bar 1+2*3, (1+2)*3"

src="""
 if cond1 then whenTrue1
 else whenFalse1
 if cond1 then whenTrue21; whenTrue22 else whenFalse2
 if cond3
    whenTrue31 + 2*3
    whenTrue32 4
 else
    whenFalse31
    whenFalse32
 for x = 1 to 10
    y = z = x+x
    print(4, 5)
"""

print "\n"+ src + "\n\n" + toIndentedString(basic.parse src) + "\n"

#print (new lex.Lexer src, basic.keywords).tokenize().join '\n'

#print basic.block

print "\n\n"

#lx=new lex.Lexer "foo", basic.keywords
#loop break unless lx.next()