gg  = require './GrammarGenerator'
lex = require './Lexer'
{ tree, toIndentedString } = require './Tree'
cs = exports

# reserved keywords
cs.keywords = new lex.Keywords( "if", "else", "true", "false", "new",
  "return", "try", "catch", "finally", "throw", "break", "continue",
  "for", "in", "while", "delete", "instanceof", "typeof", "switch",
  "super", "extends", "class", "this", "null", "debugger", "and",
  "or", "is", "isnt", "not", "then", "unless", "until", "loop", "yes",
  "no", "on", "off", "of", "by", "where", "when", "case", "default",
  "do", "function", "var", "void", "with", "const", "let", "enum",
  "export", "import", "native", "own", "__hasProp", "__extends",
  "__slice", "-=", "+=", "/=", "*=", "%=", "||=", "&&=", "?=", "<<=",
  ">>=", ">>>=", "&=", "^=", "|=", "typeof", "delete", "&&",
  "||", "<<", ">>", ">>>", "<=", ">=", "++", "--", "->", "=>", "::",
  "==", "===", "..", "...")

# Generic expression, to be populated later
cs.expr = gg.named 'expr', gg.expr()

# Single line of expressions, separated by semicolons
cs.line = gg.named 'line', gg.list(cs.expr, ';')

# Block of indentation-delimited expressions
cs.block = gg.sequence(
    gg.indent, gg.list(cs.expr, gg.choice(";", gg.newline)), gg.dedent
).setBacktrack(true).setBuilder 1

# Either a line or a block
cs.lineOrBlock = gg.named 'lineOrBlock', gg.choice(cs.block, cs.line)

# Return statement, with optional value
cs.return = gg.sequence("return", gg.maybe cs.expr).setBuilder (x) ->
    tree 'Return', x[1] or tree 'Null'

# try ... catch ... finally
cs.try = gg.sequence(
    "try", cs.block,
    gg.if('catch', gg.sequence(gg.id, cs.lineOrBlock)),
    gg.if('finally', cs.block) #lineorbblock?
).setBuilder (x) ->
    tblock = x[1]
    [_, cid, cblock] = x[2] if x[2]
    [_, fblock] = x[3] if x[3]
    return tree 'Try', tblock, cid, cblock, fblock

# while/until ... when ..., shared by block-prefix and suffix forms
cs.whileLine = gg.sequence(
    gg.choice('while', 'until'),
    cs.expr,
    gg.if('when', cs.expr)
).setBuilder (x) ->
    { cond, invert, guard } = x
    if invert then cond = tree "!", cond
    return tree [cond, guard]

# block-prefix while/until statement
cs.while = gg.sequence(
    cs.whileLine, cs.block
).setBuilder (x) ->
    [[cond, guard], body] = x
    tree "While", cond, guard, body

# block-prefix loop statement
cs.loop = gg.sequence('loop', cs.block).setBuilder (x) ->
    tree "While", (tree 'True'), x[1]

# for ... in/of ... when ... by ..., shared by block-prefix and suffix forms
cs.forLine = gg.sequence(
    "for", gg.maybe "own", gg.list(cs.expr, ','),
    gg.choice("in", "of"),
    cs.expr,
    gg.choice(
        gg.sequence(
            "when", cs.expr, gg.if("by", cs.expr)
        ).setBuilder( (x) -> guard: x[1], step: x[2] ),
        gg.sequence(
            "by", cs.expr, gg.if("when", cs.expr)
        ).setBuilder( (x) -> guard: x[2], step: x[1] )
    )
).setBuilder (x) ->
    [ _, own, vars, op, collection, { guard, step} ] = x
    return gg.fail if op=='of' and step
    return gg.fail unless 1 <= vars.length <= 2
    return [ op, vars, collection, guard, step ]

# block-prefix for statement
cs.for = gg.sequence(cs.forLine, cs.block).setBuilder (x) ->
    [t, body] = x
    return tree t..., body

# then ... or block, shared by if and switch statements
cs.thenLineOrBlock = gg.choice(
    gg.sequence("then", cs.line).setBuilder(1),
    cs.block
)

# switch statement
cs.switch = gg.sequence(
    "switch", gg.maybe cs.expr, gg.indent,
    gg.list(
        gg.sequence(
            "when", gg.list(cs.expr, ','), cs.thenLineOrBlock
        ).setBuilder(1, 2)
    ),
    gg.if("else", cs.lineOrBlock),
    gg.dedent
).setBuilder (x) ->
    [ _, test, _, cases, defaultCase, _ ] = x
    tree "Switch", test, cases, defaultCase

# if ... then ... else statement
cs.if = gg.sequence(
    "if", cs.expr,
    cs.thenLineOrBlock,
    gg.if("else", gg.choice(cs.line, cs.block))
).setBuilder (x) -> tree 'If', x[1], x[2]

# class ... extends ... block-prefix statement
cs.class = gg.sequence(
    "class", cs.expr,
    gg.if("extends", cs.expr),
    gg.maybe cs.block
).setBuilder (x) ->
    [_, className, ancestor, block ] = x
    tree 'Class', className, ancestor, block

# read an identifier, but return it as a string
cs.idAsString = gg.wrap(gg.id).setBuilder((x) -> tree 'String', x[0])

# @ and @field expressions
cs.at = gg.sequence(
    "@", gg.maybe(
        gg.sequence(gg.noSpace, cs.idAsString).setBuilder(1)
    )
).setBuilder (x) ->
    [ field ] = x
    if field then tree 'Accessor', (tree 'This'), field else tree 'This'

# Single function call argument. splats "..." are authorized only after
# parameters and arguments, in order to avoid confusion with slices.
cs.exprOrSplat = gg.sequence(
    cs.expr, gg.maybe "..."
).setBuilder (x) ->
    [expr, splat] = x
    if splat then tree 'Splat', expr else expr

# function args with explicit parentheses
cs.argumentsInParentheses = gg.sequence(
    gg.noSpace, '(', gg.list(cs.exprOrSplat, ',', "canBeEmpty"), ')'
).setBuilder(2)

# function args without parentheses
cs.argumentsWithoutParentheses = gg.sequence(
    gg.space, gg.list(cs.exprOrSplat, ',')
).setBuilder(1)

# function args
cs.arguments = gg.choice(
    cs.argumentsInParentheses,
    cs.argumentsWithoutParentheses
)

# super invocation, with or without arguments
cs.super = gg.sequence("super", gg.maybe cs.arguments).setBuilder (x) ->
    [_, args] = x
    args |= tree 'Id', arguments
    tree 'Call', (tree 'Super'),  args

# expressions starting with parentheses: normal parentheses and lambdas
cs.parentheses = gg.sequence(
    '(', gg.list(cs.exprOrSplat, ','), ')',
    gg.choice(
        ['->', cs.lineOrBlock],
        ['=>', cs.lineOrBlock],
        gg.one
    )
)
.setBuilder (x) ->
    [_, insideParens, _, optFunctionPart] = x
    if optFunctionPart
        # TODO: check that there are only parameters insideParens
        [arrow, body] = optFunctionPart
        boundFunction = (arrow=='=>')
        [g, code] = optFunctionPart
        if arrow=='->' then return tree 'Function', insideParens, body
        else return tree 'BoundFunction', insideParens, body
    else if insideParens.length == 1 # TODO: forbid splats?
        return insideParens[0]
    else
        return gg.fail

# accessor suffix.
# TODO: handle slices
cs.bracketAccessor = gg.sequence(
    gg.noSpace, '[', cs.expr,
    gg.choice(
        gg.sequence('...', cs.expr),
        gg.sequence('..', cs.expr),
        gg.one
    ),
    ']'
).setBuilder (x) ->
    [_, _, a, rest, _] = x
    if rest then [op, b] = rest; return [ a, op, b ]
    else return [ a ]

cs.field = gg.choice(gg.id, gg.anyKeyword)

# object.field
cs.dotAccessor = gg.sequence(".", cs.field).setBuilder 1

optionalNewlines = # helper for class MultiLine
    gg.list(gg.choice(gg.indent, gg.dedent, gg.newline), null, 'canbeempty')

# Support for list of elements which accept arbitrary indentations between
# them. Used for array elements and function arguments.
class MultiLine extends gg.Parser
    constructor: (args) ->
        super
        { primary, prefix, suffix, separator } = args
        @seq = gg.sequence(
            prefix ? gg.one,
            optionalNewlines,
            gg.list(primary, [separator ? gg.one, optionalNewlines]),
            suffix ? gg.one,
        ).setBuilder 2
        @seq.addListener @
        @dirty=true

    reindex: ->
        @seq.reindex()
        @keys = @seq.keys
        return super

    parse: (lx) ->
        initialIndent = lx.indentation(0) # Remember indentation
        result = @seq.call lx             # Read content
        while true                        # Skip closing dedents
            tok = lx.peek()
            if tok.t=='dedent' and tok.v>initialIndent then lx.next() else break
        return result

    toString: ->
        @seq.toString().replace /^Sequence/, 'MultiLine'


cs.multiLine = (args) -> new MultiLine args

cs.array = cs.multiLine {
    prefix:    '['
    primary:   cs.exprOrSplat
    separator: gg.choice(',', ';', gg.one)
    suffix:    ']' }
.setBuilder (x) -> tree 'Array', x

cs.object = cs.multiLine {
    prefix:    '{'
    primary:   gg.sequence(gg.id, ':', cs.expr).setBuilder (x) -> [x[0], x[2]]
    separator: gg.choice(',', ';', gg.one)
    suffix:    '}' }
.setBuilder (x) -> tree 'Object', x

cs.string = gg.wrap(gg.string).setBuilder((x) -> tree 'String', x)

cs.interpString = gg.sequence(
    gg.interpStart,
    gg.list(
        gg.choice(
            gg.sequence(gg.interpEsc, cs.expr, gg.interpUnesc).setBuilder(1),
            cs.string)
    ).setBuilder((x) -> tree '+', x...),
    gg.interpEnd
).setBuilder 1

# primary expression. prefix / infix / suffix operators will be
# added in cs.expr over this primary parser.
cs.primary = gg.named 'primary-expr', gg.choice(
    (gg.wrap gg.number).setBuilder (x) -> tree 'num', x
    (gg.wrap gg.id)    .setBuilder (x) -> tree 'id', x
    #gg.regexp,
    cs.string,
    cs.interpString,
    #gg.js,
    cs.array,
    cs.object,
    gg.wrap("true").setBuilder(-> tree 'True'),
    gg.wrap("false").setBuilder(-> tree 'False'),
    gg.wrap("break").setBuilder(-> tree 'Break'),
    gg.wrap("continue").setBuilder(-> tree 'Continue'),
    cs.if,
    cs.try,
    cs.while,
    cs.loop,
    cs.for,
    cs.switch,
    cs.class,
    cs.super,
    cs.parentheses,
    cs.at
)

cs.expr.setPrimary cs.primary

# helpers for operator declarations
# tree "Op" operators:
# +  -  ++  --  *  /  %  <<  >>  >>>
# !=  ==  <  >  <=  >=
# &&  ||  &  |  ^
# in  of  instanceof  !in  !of  !instanceof


# These functors automate the generation of builders
# based on tree().
# This musn't be done in GrammarGenerator, as
# GG doesn't and shouldn't depend on tree.

prefix = (r) ->
    {parser, builder} = r
    if not builder? and typeof parser is 'string'
        r.builder = (_,e) -> tree 'Op', parser, e
    else if typeof builder is 'string'
        r.builder = (_,e) -> tree 'Op', builder, e
    cs.expr.addPrefix r

infix = (r) ->
    {parser, builder} = r
    if not builder? and typeof parser is 'string'
        r.builder = (a,_,b) -> tree 'Op', parser, a, b
    else if typeof builder is 'string'
        r.builder = (a,_,b) -> tree 'Op', builder, a, b
    cs.expr.addInfix r

suffix = (r) ->
    {parser, builder} = r
    if not builder? and typeof parser is 'string'
        r.builder = (e,_) -> tree 'Op', parser, e
    else if typeof builder is 'string'
        r.builder = (e,_) -> tree 'Op', builder, e
    cs.expr.addSuffix r

suffix parser:[gg.noSpace, '?'], prec:190, builder:'?'

# Default '?' infix op
infix  parser:[gg.space, '?'],         prec:80,  assoc:'left', builder:'?'
infix  parser:gg.choice('is',   '=='), prec:110, assoc:'left', builder:'=='
infix  parser:gg.choice('isnt', '!='), prec:110, assoc:'left', builder:'!='
infix  parser:gg.choice('and',  '&&'), prec:100, assoc:'left', builder:'&&'
infix  parser:gg.choice('or',   '||'), prec:90,  assoc:'left', builder:'||'
prefix parser:gg.choice('not',  '!'),  prec:180, assoc:'left', builder:'!'
suffix parser:'++', prec:180, builder:'++suffix'
suffix parser:'--', prec:180, builder:'--suffix'

# Disabled for now, Node.coffee flattens binary trees to generate comparison chains:

# cs.comparators = gg.choice(
#     '<', '>', '<=', '>=', '==', '!=',
#     gg.wrap('is').setBuilder(->'=='),
#     gg.wrap('isnt').setBuilder(->'!=')
# )

# infix parser:cs.comparators, prec:130, assoc:'flat', builder:(operands, operators) ->
#     print "operators = #{operators}\n"
#     print "operands = #{operands}\n"
#     pairs = (tree 'op', operators[i], operands[i], operands[i+1] for i in [0...operators.length])
#     if pairs.length==1 then pairs[0]
#     else tree 'op', '&&', pairs...

# Operators whose concrete syntax names match AST 'Op' tag.
regularOperators = [
    # 190: suffix '?'
    [ prefix, {prec:180}, '+', '-', '!', '~', '++', '--' ],
    [ infix,  {prec:170}, '*', '/', '%' ],
    [ infix,  {prec:160}, '+', '-' ],
    [ infix,  {prec:150}, '<<', '>>', '>>>' ],
    [ infix,  {prec:140}, '&', '|', '^' ],
    [ infix,  {prec:130}, '<=', '<', '>', '>=' ], # TODO convert into chainable operators
    [ infix,  {prec:120}, 'instanceof', 'in', 'of' ], # TODO check prec
    [ prefix, {prec:120}, 'delete', 'typeof', 'new', 'throw' ], # TODO not operators?
    # 80: infix '?', surrounded by spaces
    [ infix, {prec:70, assoc:'right'}, '=', '-=', '+=', '/=', '*=', '%=', '||=', '&&=', '?='],
]

for [f, r, parsers...] in regularOperators
    for parser in parsers
        r2 = { parser }; r2[k] = v for k, v of r
        f r2

prefix parser:'->',           prec:10, builder:(_, body) -> tree "Function", [], body
suffix parser:cs.arguments,   prec:30, builder:(f, args) -> tree "Call", f, args
suffix parser:cs.whileLine,   prec:20, builder:(x, w) -> tree "While", w[0], w[1], [x]
suffix parser:cs.dotAccessor, prec:90, builder:(x, i) -> tree "Accessor", x, tree "Value", i
suffix parser:cs.bracketAccessor, prec:90, builder:(x, i) ->
    [ a, op, b ] = i
    return tree "Accessor", x, a unless op
    return tree "RangeAccessor", x, op, a, b

infix parser:'if',     prec:60, assoc:'right', builder:(a,_,b) -> tree 'If', b, [a]
infix parser:'unless', prec:60, assoc:'right', builder:(a,_,b) -> tree 'If', (tree 'Op', '!', b), [a]

# main parsing function
cs.parse = (parser, src) ->
    if not src?
        src=parser; parser = cs.block
    else if typeof parser == 'string' and cs[parser]?
        parser = cs[parser]
    else unless parser instanceof gg.Parser
        throw new Error "bad args"
    lexer = new lex.Lexer(src, cs.keywords)
    stream = new lex.Stream lexer
    # print("\nTokens: \n#{stream.tokens.join('\n')}\n\n")
    parser.call stream

for name, p of cs
    if p instanceof gg.Parser
        gg.named "cs."+name, p
