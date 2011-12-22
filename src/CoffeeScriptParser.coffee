gg  = require './GrammarGenerator'
lex = require './Lexer'
{ tree, toIndentedString } = require './Tree'
cs = exports

{ print, error, inspect } = require 'util'

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

cs.keywords.addDetailedCatcode('keyword', 'interpStart')

# Generic expression, to be populated later
cs.expr = gg.named 'expr', gg.expr()

# Single line of expressions, separated by semicolons
cs.line = gg.named 'line', gg.list(cs.expr, ';')

# Block of indentation-delimited expressions
cs.nonEmptyBlock = gg.sequence(
    gg.indent,
    gg.list(cs.expr, gg.choice(";", gg.newline)),
    gg.dedent
).setBuilder(1)

# Block of indentation-delimited expressions
cs.block = gg.maybe(cs.nonEmptyBlock, [ ])

# Either a line or a block
cs.lineOrBlock = gg.choice(cs.line, cs.block)

# Return statement, with optional value
cs.return = gg.sequence("return", gg.maybe cs.expr).setBuilder (x) ->
    tree 'Return', x[1] or tree 'Null'

# try ... catch ... finally
cs.try = gg.sequence(
    "try", cs.lineOrBlock,
    gg.if('catch', gg.sequence(gg.id, cs.lineOrBlock)),
    gg.if('finally', cs.lineOrBlock)
).setBuilder (x) ->
    [_, tblock, cpair, fblock] = x
    [cid, cblock] = cpair or [ ]
    return tree 'Try', tblock, cid, cblock, fblock

# while/until ... when ..., shared by block-prefix and suffix forms
cs.whileLine = gg.sequence(
    gg.choice('while', 'until'),
    cs.expr,
    gg.if('when', cs.expr)
).setBuilder (x) ->
    [ keyword, cond, guard ] = x
    if keyword is 'until' then cond = tree 'Op', '!', cond
    if guard then return [cond, guard]
    else return [cond]

# block-prefix while/until statement
cs.while = gg.sequence(
    cs.whileLine, cs.nonEmptyBlock
).setBuilder (x) ->
    [ line, body ] = x
    return tree 'While', line..., body

# block-prefix loop statement
cs.loop = gg.sequence('loop', cs.block).setBuilder (x) ->
    tree "While", (tree 'True'), x[1]

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
    if splat then tree 'Op', '...', expr else expr

buildSplat = (x) ->
    [expr, splat] = x
    if splat then tree 'Op', '...', expr else expr

# function args with explicit parentheses
cs.argumentsWithParentheses = gg.sequence(
    gg.noSpace,
    '(',
    gg.maybeList(
        gg.sequence(
            cs.expr,
            gg.maybe '...'
        ).setBuilder(buildSplat),
        ','
    ),
    ')'
).setBuilder(2)

# function args without parentheses
cs.argumentsWithoutParentheses = gg.sequence(
    gg.space,
    gg.list(
        gg.sequence(
            gg.wrap(cs.expr, 30),
            gg.maybe '...'
        ).setBuilder(buildSplat)
        ','
    ),
).setBuilder(1)

# Disabled: both forms have different precedences
# function args
#cs.arguments = gg.choice(
#    cs.argumentsWithParentheses,
#    cs.argumentsWithoutParentheses
#)

# super invocation, with or without arguments
cs.super = gg.lift("super").setBuilder -> tree 'Super'


# expressions starting with parentheses: normal parentheses and lambdas
cs.parentheses = gg.sequence(
    '(', gg.maybeList(cs.exprOrSplat, ','), ')',
    gg.choice(
        ['->', cs.lineOrBlock],
        ['=>', cs.lineOrBlock],
        gg.one
    )
)
.setBuilder (x) ->
    [_, insideParens, _, optFunctionPart] = x
    if optFunctionPart

        # Check param validity, find splat index if appropriate
        for param, i in insideParens
            if param.tag=='Id'
                continue
            else if param.tag=='Op' and param.children[0]=='...' and param.children[1].tag=='Id'
                if splatIdx then return gg.fail # multiple splats forbidden
                else splatIdx = i; insideParens[i]=param.children[1]
            else return gg.fail # invalid parameter

        [arrow, body] = optFunctionPart
        boundFunction = (arrow=='=>')
        [g, code] = optFunctionPart
        Tag = if arrow=='->' then 'Function' else 'Boundfunc'
        if splatIdx? then return tree Tag, insideParens, body, splatIdx
        else return tree Tag, insideParens, body
    else if insideParens.length == 1 # TODO: forbid splats?
        return insideParens[0]
    else
        return gg.fail


# expressions starting with parentheses: normal parentheses and lambdas
cs.paramlessFunc = gg.sequence(
    gg.choice('->','=>'),
    cs.lineOrBlock
)
.setBuilder (x) ->
    [ arrow, body ] = x
    Tag = if arrow=='->' then 'Function' else 'Boundfunc'
    return tree Tag, [ ], body

cs.range = gg.sequence(
    '[',
    cs.expr,
    gg.choice('...', '..'),
    cs.expr,
    ']'
).setBuilder (x) ->
    [_, a, op, b, _] = x
    tree 'Range', a, op, b

cs.field = gg.choice(gg.id, gg.anyKeyword)

# object.field
cs.dotAccessor = gg.sequence(".", cs.field).setBuilder 1

optionalNewlines = # helper for class MultiLine
    gg.maybeList(gg.choice(gg.indent, gg.dedent, gg.newline))

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
        @catcodes = @seq.catcodes
        @epsilon  = @seq.epsilon
        return super

    parseInternal: (lx) ->
        initialIndent = lx.indentation(0) # Remember indentation
        result = @seq.parse lx            # Read content
        while true                        # Skip closing dedents
            tok = lx.peek()
            if tok.t=='dedent' and tok.v>initialIndent then lx.next() else break
        return result

    toString: ->
        @seq.toString().replace /^Sequence/, 'MultiLine'


cs.multiLine = (args) -> new MultiLine args

cs.array = (cs.multiLine {
    prefix:    '['
    primary:   cs.exprOrSplat
    separator: gg.choice(',', ';', gg.one)
    suffix:    ']' })
.setBuilder (x) -> tree 'Array', x

cs.objectEntry =
    gg.sequence(
        gg.id,
        gg.if(':', cs.expr)
    ).setBuilder (x) ->
        [label, value] = x
        [label, value ? label]

cs.objectWithoutBraces = cs.multiLine({
    primary:   cs.objectEntry
    separator: gg.choice(',', ';', gg.one)
}).setBuilder (x) -> tree 'Object', x

cs.objectWithBraces = cs.multiLine({
    prefix:    "{"
    primary:   cs.objectEntry
    separator: gg.choice(',', ';', gg.one)
    suffix:    "}"
}).setBuilder (x) -> tree 'Object', x

cs.object = gg.choice(cs.objectWithoutBraces, cs.objectWithBraces)
#cs.object = cs.objectWithBraces

cs.id     = gg.wrap(gg.id).setBuilder (x) -> tree 'Id', x
cs.number = gg.wrap(gg.number).setBuilder (x) -> tree 'Number', x
cs.string = gg.wrap(gg.string).setBuilder((x) -> tree 'String', x)

cs.regex = gg.sequence(
    gg.regex,
    gg.maybe (gg.regexFlags)
).setBuilder (x) ->
    [regex, flags] = x
    if flags then tree 'Regex', regex, flags else tree 'Regex', regex

cs.interpString = gg.sequence(
    gg.interpStart('"', '"""'),
    gg.list(
        gg.choice(
            gg.sequence(gg.interpEsc, cs.expr, gg.interpUnesc).setBuilder(1),
            cs.string)
    ).setBuilder((x) -> tree 'Op', '+', x...),
    gg.interpEnd
).setBuilder 1

cs.interpRegex = gg.sequence(
    gg.interpStart('///'),
    gg.list(
        gg.choice(
            gg.sequence(gg.interpEsc, cs.expr, gg.interpUnesc).setBuilder(1),
            gg.wrap(gg.regex).setBuilder (x) -> tree 'String', x)
    )
    gg.interpEnd,
    gg.maybe(gg.regexFlags)
).setBuilder (x) ->
    [ _, content, _, flags] = x
    r = tree 'Op', 'new', (tree 'Id', 'RegExp'), (tree 'Op', '+', content...)
    if flags then r.children.push (tree 'String', flags)
    return r

# for ... in/of ... when ... by ..., shared by block-prefix and suffix forms
cs.forLine = gg.sequence(
    "for", gg.maybe("own"),
    gg.list(
        #gg.wrap(cs.expr,200),
        gg.choice(
            cs.id,
            cs.object,
            cs.array
        ),
        ','
    ),
    gg.choice("in", "of"),
    cs.expr,
    gg.maybe(
        gg.choice(
            gg.sequence(
                "when", cs.expr, gg.if("by", cs.expr)
            ).setBuilder( (x) -> guard: x[1], step: x[2] ),
            gg.sequence(
                "by", cs.expr, gg.if("when", cs.expr)
            ).setBuilder( (x) -> guard: x[2], step: x[1])
        ),
        { }
    )
).setBuilder (x) ->
    [ _, own, vars, op, collection, {guard, step} ] = x
    return gg.fail if op=='of' and step
    return gg.fail if op=='in' and own
    return gg.fail unless 1 <= vars.length <= 2
    r = [ vars, op, collection ]
    r.push guard if guard or step
    r.push step  if step
    #print "FORLINE: #{r}\n"
    return r

# block-prefix for statement
cs.for = gg.sequence(cs.forLine, cs.nonEmptyBlock).setBuilder (x) ->
    [t, body] = x
    return tree 'For', t..., body

# primary expression. prefix / infix / suffix operators will be
# added in cs.expr over this primary parser.
cs.primary = gg.named 'primary-expr', gg.choice(
    310,
    cs.number,
    cs.id,
    cs.string,
    cs.interpString,
    cs.regex,
    cs.interpRegex,
    #gg.js,
    cs.array,
    gg.wrap("true").setBuilder(-> tree 'True'),
    gg.wrap("false").setBuilder(-> tree 'False'),
    gg.wrap("break").setBuilder(-> tree 'Break'),
    gg.wrap("continue").setBuilder(-> tree 'Continue'),
    cs.try,
    cs.loop,
    cs.switch,
    cs.class,
    cs.super,
    cs.parentheses,
    cs.paramlessFunc,
    cs.at,
    300,
    cs.object, # lower precedence, it's ambiguous with gg.id
    20,
    cs.if,
    cs.while,
    cs.for,
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
    else if builder not instanceof Function
        throw new Error "missing builder for prefix #{r}"
    cs.expr.addPrefix r

infix = (r) ->
    {parser, builder} = r
    if not builder? and typeof parser is 'string'
        r.builder = (a,_,b) -> tree 'Op', parser, a, b
    else if typeof builder is 'string'
        r.builder = (a,_,b) -> tree 'Op', builder, a, b
    else if builder not instanceof Function
        throw new Error "missing builder for infix #{r}"
    cs.expr.addInfix r

suffix = (r) ->
    {parser, builder} = r
    if not builder? and typeof parser is 'string'
        r.builder = (e,_) -> tree 'Op', parser, e
    else if typeof builder is 'string'
        r.builder = (e,_) -> tree 'Op', builder, e
    else if builder not instanceof Function
        throw new Error "missing builder for suffix #{r}"
    cs.expr.addSuffix r

suffix parser:[gg.noSpace, '?'], prec:190, builder:'?'

# Default '?' infix op
infix  parser:[gg.space, '?'],         prec:80,  assoc:'left', builder:'?'
infix  parser:gg.choice('is',   '=='), prec:110, assoc:'left', builder:'=='
infix  parser:gg.choice('isnt', '!='), prec:110, assoc:'left', builder:'!='
infix  parser:gg.choice('and',  '&&'), prec:100, assoc:'left', builder:'&&'
infix  parser:gg.choice('or',   '||'), prec:90,  assoc:'left', builder:'||'
prefix parser:gg.choice('not',  '!'),  prec:180, assoc:'left', builder:'!'
prefix parser:'++', prec:180, builder:'++prefix'
prefix parser:'--', prec:180, builder:'--prefix'
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
    [ prefix, {prec:180}, '+', '-', '!', '~' ],
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

for [adder, template, parsers...] in regularOperators
    for parser in parsers
        descr = { parser }
        (descr[k] = v) for k, v of template
        adder descr

suffix
    parser:cs.argumentsWithParentheses, prec:200,
    builder:(f, args) -> tree "Call", f, args
suffix
    parser:cs.argumentsWithoutParentheses, prec:200,
    builder:(f, args) -> tree "Call", f, args
suffix
    parser:cs.whileLine, prec:20,
    builder:(x, w) -> tree "While", w..., [x]
infix
    parser:'if', prec:20, assoc:'left',
    builder:(a,_,b) -> tree 'If', b, [a]
infix
    parser:'unless', prec:20, assoc:'left',
    builder:(a,_,b) -> tree 'If', (tree 'Op', '!', b), [a]
suffix
    parser:cs.forLine, prec:20, builder:(e, f) -> tree 'For', f..., [e]
suffix
    parser:cs.whileLine, prec:20, builder:(e, f) -> tree f..., [e]
suffix
    parser:cs.dotAccessor, prec:90,
    builder:(x, i) -> tree "Accessor", x, (tree 'String', i)
suffix
    parser:[gg.noSpace, cs.range], prec:90,
    builder: (x, i) -> tree 'Slice', x, i[1]
suffix
    parser:[gg.noSpace, '[', cs.expr, ']'], prec:90,
    builder: (x, y) -> tree 'Accessor', x, y[2]

# main lexing function
cs.lex = (src) ->
    lexer  = new lex.Lexer(src, cs.keywords)
    stream = new lex.Stream lexer
    print("\nTokens: \n#{stream.tokens.join('\n')}\n\n")
    stream

# main parsing function
cs.parse = (parser, src) ->
    if not src?
        src=parser; parser = cs.nonEmptyBlock
    else if typeof parser == 'string' and cs[parser]?
        parser = cs[parser]
    else unless parser instanceof gg.Parser
        throw new Error "bad args"
    stream = cs.lex src
    parser.parse stream

for name, p of cs
    if p instanceof gg.Parser
        gg.named "cs."+name, p
