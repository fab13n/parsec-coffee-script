gg  = require './GrammarGenerator'
lex = require './Lexer'
{ tree, toIndentedString } = require './Tree'
cs = exports

# reserved keywords
cs.keywords = new lex.Keywords(
  "if", "else", "true", "false", "new", "return", "try", "catch",
  "finally", "throw", "break", "continue", "for", "in", "while",
  "delete", "instanceof", "typeof", "switch", "super", "extends",
  "class", "this", "null", "debugger", "and", "or", "is", "isnt",
  "not", "then", "unless", "until", "loop", "yes", "no", "on", "off",
  "of", "by", "where", "when", "case", "default", "do", "function",
  "var", "void", "with", "const", "let", "enum", "export", "import",
  "native", "__hasProp", "__extends", "__slice", "-=", "+=", "/=",
  "*=", "%=", "||=", "&&=", "?=", "<<=", ">>=", ">>>=", "&=", "^=",
  "|=", "!!", "typeof", "delete", "&&", "||", "<<", ">>", ">>>", "<=",
  ">=", "++", "--", "->", "=>", "::", "==", "===", "..", "...")

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
).setBuilder (x) -> { invert: x[0]=='until', cond:x[1], guard:x[2] }

# block-prefix while/until statement
cs.while = gg.sequence(
    cs.whileLine, cs.block
).setBuilder (x) ->
    [w, body] = x
    tree "While", w.cond, w.invert, w.guard, body

# block-prefix loop statement
cs.loop = gg.sequence('loop', cs.block).setBuilder (x) ->
    tree "While", (tree 'True'), false, x[1]

# for ... in/of ... when ... by ..., shared by block-prefix and suffix forms
cs.forLine = gg.sequence(
    "for", gg.maybe "all", gg.list(cs.expr, ','),
    gg.choice("in", "of"),
    cs.expr,
    gg.choice(
        gg.sequence(
            "when", cs.expr, gg.if("by", cs.expr)
        ).setBuilder( (x) -> when: x[1], by: x[2] ),
        gg.sequence(
            "by", cs.expr, gg.if("when", cs.expr)
        ).setBuilder( (x) -> when: x[2], by: x[1] )
    )
).setBuilder (x) ->
    r = { }
    [ _, r.all, r.vars, r.op, r.collection, r.deco ] = x
    return gg.fail if r.op=='of' and r.deco.by
    return gg.fail unless 1 <= r.vars.length <= 2
    return tree(
        "For", r.all=='all', r.vars, r.op=='of',
        r.collection, r.deco.when, r.deco.by
    )

# block-prefix for statement
cs.for = gg.sequence(cs.forLine, cs.block).setBuilder (x) ->
    [t, body] = x
    t.push body
    return t

# then ... or block, shared by if and switch statements
cs.thenLineOrBlock = gg.choice(
    gg.sequence("then", cs.line).setBuilder 1,
    cs.block
)

# switch statement
cs.switch = gg.sequence(
    "switch", gg.maybe cs.expr, gg.indent,
    gg.list(
        gg.sequence(
            "when", gg.list(cs.expr, ','), cs.thenLineOrBlock
        ).setBuilder (x) -> [x[1], x[2]]
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
).setBuilder (x) -> tree 'Class', x[1], x[2], x[3]

# @ and @field expressions
cs.at = gg.sequence(
    "@", gg.maybe [gg.noSpace, gg.id]
).setBuilder (x) ->
    if x[1] then tree 'Accessor', (tree 'This'), x[1][1]
    else tree 'This'

# Single function call argument. splats "..." are authorized only after
# parameters and arguments, in order to avoid confusion with slices.
cs.exprOrSplat = gg.sequence(
    cs.expr, gg.maybe "..."
).setBuilder (x) -> if x[1] then tree 'Splat', x[0] else x[0]

# function args with explicit parentheses
cs.argumentsInParentheses = gg.sequence(
    gg.noSpace, '(', gg.list(cs.exprOrSplat, ',', "canBeEmpty"), ')'
).setBuilder 2

# function args without parentheses
cs.argumentsWithoutParentheses = gg.sequence(
    gg.space, gg.list(cs.exprOrSplat, ',')
).setBuilder 1

# function args
cs.arguments = gg.choice(
    cs.argumentsInParentheses,
    cs.argumentsWithoutParentheses
)

# super invocation, with or without arguments
cs.super = gg.sequence("super", gg.maybe cs.arguments).setBuilder (x) ->
    tree 'Call', 'super',  x[1] or [tree 'Splat', (tree 'Literal', 'arguments')]

# expressions starting with parentheses: normal parentheses and lambdas
cs.parentheses = gg.sequence(
    '(', gg.list(cs.exprOrSplat, ','), ')',
    gg.choice(
        ['->', cs.lineOrBlock],
        ['=>', cs.lineOrBlock],
        gg.one
    )
).setBuilder (x) ->
    [_, content, _, glyphAndCode] = x
    if glyphAndCode
        [glyph, code] = glyphAndCode
        tag = if glyph=='->' then 'Function' else 'BoundFunction'
        return tree tag, content, code
    else if content.length != 1 # TODO: forbid splats?
        return gg.fail
    else return content[0]

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

# object.field
cs.dotAccessor = gg.sequence(
    ".", gg.choice(gg.id, gg.anyKeyword)
).setBuilder 1


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

# primary expression. prefix / infix / suffix operators will be
# added in cs.expr over this primary parser.
cs.primary = gg.named 'primary-expr', gg.choice(
    (gg.wrap gg.number).setBuilder (x) -> tree 'num', x
    (gg.wrap gg.id)    .setBuilder (x) -> tree 'id', x
    #gg.regexp,
    #gg.string, not implemented
    #gg.js,
    cs.array,
    cs.object,
    gg.wrap("true").setBuilder(-> tree 'Literal', 'true'),
    gg.wrap("false").setBuilder(-> tree 'Literal', 'false'),
    gg.wrap("break").setBuilder(-> tree 'Literal', "break"),
    gg.wrap("continue").setBuilder(-> tree 'Literal', "continue"),
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

prefix = (op, prec, builder) ->
    if not builder?
        builder = op
    if typeof builder is 'string'
        tag = builder; builder = (_, a) -> tree tag, a
    cs.expr.addPrefix {parser:op, prec, builder}

infix  = (op, prec, assoc, builder) ->
    if not builder?
        builder = op
    if typeof builder is 'string'
        tag = builder; builder = (a, _, b) -> tree tag, a, b
    cs.expr.addInfix { parser:op, prec, assoc: assoc ? 'left', builder }

suffix = (op, prec, builder) ->
    if not builder? and typeof op is 'string'
        builder = op
    if typeof builder is 'string'
        tag = builder; builder = (a, _) -> tree tag, a
    cs.expr.addSuffix {parser:op, prec, builder}

# Existential
suffix [gg.noSpace, '?'], 190, 'Existence'

# Default '?' infix op
infix [gg.space, '?'], 80, 'left', 'Default'

infix  'is',   110, 'left', '=='
infix  'isnt', 110, 'left', '!='
infix  'and',  110, 'left', '&&'
infix  'or',   110, 'left', '||'
prefix 'not',  180, '!'

suffix '++', 180, '++suffix'
suffix '--', 180, '--suffix'

regularOperators = [
    # 190: suffix '?'
    [ prefix, [180], '+', '-', '!', '!!', '~', '++', '--' ],
    [ infix,  [170], '*', '/', '%' ],
    [ infix,  [160], '+', '-' ],
    [ infix,  [150], '<<', '>>', '>>>' ],
    [ infix,  [140], '&', '|', '^' ],
    [ infix,  [130], '<=', '<', '>', '>=' ], # TODO convert into chainable operators
    [ prefix, [120], 'delete', 'instanceof', 'typeof', 'new', 'throw' ],
    [ infix,  [110], '==', '!=' ], # plus aliases 'is', 'isnt' TODO and chaining
    [ infix,  [100], '&&' ], # plus alias 'and'
    [ infix,   [90], '||' ], # plus alias 'or'
    # 80: infix '?', surrounded by spaces
    [ infix, [70, 'right'], '=', '-=', '+=', '/=', '*=', '%=', '||=', '&&=', '?='],
]

parseRegularOperatorsLists = ->
    for [f, rest, firsts...] in regularOperators
        for first in firsts
            f(first, rest...)

parseRegularOperatorsLists()

prefix '->', 10, (_, body) -> tree "Function", [], body

suffix cs.arguments,       30, (f, args) -> tree "Call", f, args, false
suffix cs.whileLine,       20, (x, w) -> tree "While", w.cond, w.invert, w.guard, [x]
suffix cs.dotAccessor,     90, (x, i) -> tree "Accessor", x, tree "Value", i
suffix cs.bracketAccessor, 90, (x, i) ->
    [ a, op, b ] = i
    return tree "Accessor", x, a unless op
    return tree "RangeAccessor", x, op, a, b

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

