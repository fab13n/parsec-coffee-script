#
# Grammar generator
#

L = require './log'
util=require 'util'

this.exports = this unless process?

fail = exports.fail = { toString: -> "<FAIL>" }

lastUid=1

#-------------------------------------------------------------------------------
# Check that x is either a parser, or something that can be sensibly
# converted into a parser.
# Return a parser or throws an error.
#
# Supported lifting operations:
#  * strings are lifted into keywords;
#  * arrays are lifted into sequences (and their content recursively lifted);
#  * functions are lifted into parsers.
#-------------------------------------------------------------------------------
lift = exports.lift = (x) ->
    return     x                 if x instanceof Parser
    return new Wrap(x...)        if x instanceof Array and x.length==1
    return new Sequence(x...)    if x instanceof Array
    return new LiftedFunction(x) if x instanceof Function
    return     keyword(x)        if typeof x is 'string'
    return     one               if x == 1
    return     zero              if x == 0
    throw  new Error "null argument to parser" unless x
    throw  new Error "Parser expected, got #{x.toString()}"

#-------------------------------------------------------------------------------
# Common ancestor for all parsers.
#
# Internal structure:
#
# catcodes: Optional set of token catcodes with which this parser might start.
# If there is a set, then it must be exhaustive:
#
#  * it is always OK for a parser to have @catcodes=false, although it might
#    make it inefficient, especially when used in a Choice combinator;
#
#  * it is OK for a parser to declare, say, "keyword-foo" as a key, but
#    to fail with some token streams starting with keyword "foo";
#
#  * but it is an error for a parser to have a @catcodes set that doesn't
#    include "keyword-foo" if there are some token streams starting with
#    keyword "foo" that it might parse successfully.
#
#  * as a corollary, a parser with an empty set of catcodes (as opposed to
#    no set at all, @catcodes==false), promizes never to succeed parsing
#    anything. It might be the case for a parser in which we plan to
#    add some children some time later.
#
# transformers: list of functions applied in sequence to the result of
# the parsing. Allows to add post-processing in the AST generation.
#
# builder: generate the result from the bits. What constitutes the "bits"
# depends on the parser class.
#
# listeners: list of parsers to be notified when this parser is updated.
# This is intended to propagate changes in key sets.
#
# backtrack: when false, failing to parse causes an error instead of returning
# 'fail'
#
#-------------------------------------------------------------------------------
callNest    = 0
reindexNest = 0

exports.Parser = class Parser
    typename: "Parser"

    constructor: ->
        @builder      ?= (x) -> x
        @transformers ?= [ ]
        @catcodes     ?= false
        @epsilon      ?= false
        @listeners    ?= [ ]
        @uid          ?= lastUid++
        @dirty        ?= true

    # Run the parser on the token stream `lx', consumming tokens out of it
    # if applicable. Return the object `fail' and leaves the token stream
    # unchanged if parsing fails.
    parse: (lx, args...) ->

        if typeof lx is 'string'
            if exports.defaultStreamMaker
                lx = exports.defaultStreamMaker lx
            else
                @error "token stream expected"

        isIndexed = if @dirty then "[?]" else if @catcodes then "[+]" else "[-]"
        L.logindent 'pcall',
            "? #{@toShortString(80)}, next token = #{lx.peek()} #{isIndexed}"

        if @dirty then @reindex()

        x = @parseInternal(lx, args...)

        if x==fail
            L.logdedent('pcall', "- #{@toShortString()} failed on #{lx.peek().getCatcode()}.")
            return fail
        else
            if @builder? then x = @builder x
            (x = t(x)) for t in @transformers
            L.logdedent('pcall', "+ #{@toShortString()} succeeded, returned '#{x}'.")
            return x

    # Internal parsing method: return either a result or `fail', by consumming
    # tokens from lx.
    # This is a protected method which must not be called from outside: use
    # the wrapping method @parse(lx) instead.
    parseInternal: (lx) -> @error "Invalid parser"

    # Change the builder. Argument can be:
    #  * a builder function;
    #  * a number n: the builder selects the nth element of a list
    setBuilder: (builders...) ->
        [builder] = builders
        if typeof builder == 'number'
            if builders.length>1
                indices=builders
                @builder = (x) -> r=[ ]; r.push x[i] for i in indices; r
            else n=builder; @builder = (x) -> x[n]
        else if builder instanceof Function then @builder = builder
        else k=builder; @builder = ->k
        return @

    addTransformer: (t...) ->
        @transformers = @transformers.concat t
        return @

    reindex: ->
        return @ unless @dirty

        L.logindent 'reindex', "/ reindex '#{@toShortString()}'"
        @reindexInternal()
        @dirty=false
        L.logdedent 'reindex',
            "\\ reindex '#{@toShortString()}', catcodes=#{@catcodes2string()}"
        return @

    # Perform all operations required to compute this parser's set of catcodes,
    # and its epsilon status.
    #
    # This might require the reindexing of some children, but should
    # not cause a full reindexing if not needed for catcodes:
    #
    # - reindexing will be triggered by @parse() if and when needed.
    #
    # - Moreover, a too eager recursive reindexing might cause
    #   infinite loops (this is closely related to left-recursion
    #   issues with top-down parsers).
    #
    # TODO: add support for constant parsers, to optimize reindexing when applicable
    reindexInternal: ->

    # When a change is made to this parser, notify all parsers who
    # registered for update notifications.
    # Notification procedure:
    # - a parent parser P's catcodes depend on its child parser C to determine its
    #   catcodes, and a change in C's catcodes might cause a change in P's catcodes.
    # - P informs C that it needs to be notified about catcodes changes, by
    #   calling C.addListener(P).
    # - C is subjected to an operation which changes its catcodes. It recomputes
    #   its own catcodes, and notifies it listeners, including P.
    # - P receives the notification, updates its catcodes in its implementation
    #   of @notify; if its own catcodes have changed, it propagates the notification
    #   through a final supernotify().
    #
    # TODO: delay notification until the first parsing occurs, to avoid
    #       useless multiple notifications. Use a "dirty" flag instead,
    #       and perform a check in @parse
    notify: ->
        return if @dirty
        @dirty=true
        listener.notify() for listener in @listeners

    # Register another parser to be notified when this one is updated.
    addListener: (p) ->
        # This protection isn't sufficient, it won't
        # detect cycles longer than 2.
        @error "mutual dependency" if p.isListenedBy @
        @listeners.push p unless @isListenedBy p
        return @

    isListenedBy: (p) ->
        # TODO: only checks cycles of length 1
        (return true if p==q) for q in @listeners
        return false

    toString: -> @name ? "#{@typename}"

    # Limit the maximum size of the parser's @toString result, introducing
    # an elipsis "..." if necessary. The 'max' parameter must be at least 3.
    toShortString: (max) ->
        max ?= 32
        longString = @toString()
        if longString.length>max
            return longString[0...max-3] + "..."
        else return longString

    error: (msg) -> throw new Error @toString()+": ParsingError: "+msg

    catcodes2string: ->
        if @catcodes
            "{ " + ("'#{k.replace /^keyword\-/, '!'}'" for k of @catcodes).join(", ") + " }"
        else
            "NOCATCODES"


#-------------------------------------------------------------------------------
# Wrap a function into a parser, so that it respects the parser API:
# (called via method .parse(lx), supporting transformers list).
#-------------------------------------------------------------------------------
exports.liftedFunction = (x...) -> new LiftedFunction x...
exports.LiftedFunction = class LiftedFunction extends Parser
    # field f: function to be applied
    typename:    'Function'
    constructor: (@f) -> super
    parseInternal:       (lx) -> return @f(lx)


#-------------------------------------------------------------------------------
# Match a token of type t, return it on success.
#-------------------------------------------------------------------------------
exports.Const = class Const extends Parser

    # field t: expected token type
    # field values: optional set of accepted token values
    typename: 'Const'

    # t: type of token
    # valueKeyed: if true, the value is expected to be included in the key.
    constructor: (@t, valueKeyed, values...) ->
        super
        if values.length>0
            @values = { }
            (@values[x]=true) for x in values

        @catcodes = { }
        if valueKeyed?
            if @t=='keyword' then @name='!'+values.join '-'
            else @name = @t + '-' + values.join '-'
            (@catcodes[@t+'-'+v] = true) for v in values
        else
            @name = @t
            @catcodes[@t] = true
        @dirty = false

    parseInternal: (lx) ->
        tok = lx.peek()
        if tok.t != @t
            return fail
        else if @values? and not @values[tok.v]
            return fail
        else
            return lx.next().v

# TODO: put in a for loop
exports.id         = new Const 'id'
exports.number     = new Const 'number'
exports.indent     = new Const 'indent'
exports.dedent     = new Const 'dedent'
exports.newline    = new Const 'newline'
exports.javascript = new Const 'javascript'
exports.interpStart= new Const 'interpStart'
exports.interpEnd  = new Const 'interpEnd'
exports.interpEsc  = new Const 'interpEsc'
exports.interpUnesc= new Const 'interpUnesc'
exports.javascript = new Const 'javascript'
exports.string     = new Const 'string'
exports.keyword    = keyword = (values...) -> new Const 'keyword', true, values...

#-------------------------------------------------------------------------------
# Read any keyword.
#-------------------------------------------------------------------------------
exports.AnyKeyword = class AnyKeyword extends Parser
    typename: "any-keyword"
    constructor: -> super; @dirty = false
    parseInternal: (lx) ->
        tok = lx.peek()
        if tok.t == 'keyword' then return lx.next().v
        else return fail

exports.anyKeyword = new AnyKeyword()

#-------------------------------------------------------------------------------
# Compose several parsers in a sequence.
#-------------------------------------------------------------------------------
exports.sequence = (x...) -> new Sequence x...
exports.Sequence = class Sequence extends Parser

    # field children: list of sub-parsers composing the sequence

    constructor: (children...) ->
        super
        @children = (lift child for child in children)
        @dirty=true
        @backtrack ?= true

        # TODO: no need to listen after the epsilon children.
        # However, the number of epsilon children might change,
        # and the epsilon property is only known after reindexing.
        for child in @children
            child.addListener @
            #break if child.epsilon

    setBacktrack: (@backtrack) -> @

    reindexInternal: ->
        @catcodes = { }
        @epsilon  = false
        for child in @children
            child.reindex()
            continue if child.epsilon == 'always'
            if child.catcodes
                (@catcodes[k] = true) for k of child.catcodes
                unless child.epsilon=='maybe'
                    @epsilon = false
                    break
            else
                L.log 'algo', "Sequence #{@} loses catcode because of #{child}!"
                @catcodes = false
                break

    parseInternal: (lx) ->
        result   = []
        bookmark = lx.save()
        for child, i in @children
            L.log 'sequence', "Sequence child ##{i}"
            x = child.parse(lx)
            if x == fail
                if i isnt 0 and not @backtrack
                    @error "failed on element ##{i}"
                else
                    logPrevToken = lx.peek()
                    lx.restore bookmark
                    L.log 'sequence', ">>>> BACKTRACKING FROM #{
                        logPrevToken} TO #{lx.peek()} in #{@toShortString()} <<<<<" if i>0
                    return fail
            else result.push x
        return result

    toString: -> @name ? "Sequence(#{@children.join ', '})"


#-------------------------------------------------------------------------------
# Choose between alternative parsers, according to the first token's key.
#
# Optionally, takes an extra precedence parameter: children with precedence
# below this one won't be chosen.
#
#-------------------------------------------------------------------------------
exports.choice = choice = (x...) -> new Choice x...
exports.Choice = class Choice extends Parser

    # Children parsers are encapsulated into 'entry' record
    # structures, with fields 'parser', 'prec' (the precedence of the
    # child within the choice parser), 'assoc'.
    #
    # @list: unordered list of entries, containing all the children of
    # this parser. This list is not used directly during parsing, it is used
    # by @reindex to fill the two other lists.
    #
    # @indexed: hashmap of lists of entries, indexed by catcode; each
    # list is sorted by decreasing precedence. In the list indexed by
    # catcode 'cc' are all children that might succeed when the next
    # token has catcode cc, i.e. all the children which have catcode
    # cc plus all the children for which @catcode is false.
    #
    # @unindexed: list of entries for which children have
    # @catcode==false, ordered by decreasing precedence.

    constructor: (precsAndChildren...) ->
        super
        @list       = [ ] # all children
        @add precsAndChildren...

    # children are not put directly in the correct index. Instead,
    # the whole Choice parser is marked as dirty, so that it will be reindexed
    # correctly the first time it's used.
    add: (precsAndChildren...) ->
        prec = 50
        i = 0
        len = precsAndChildren.length
        return @ if len is 0
        while i<len
            x = precsAndChildren[i++]
            if typeof x == 'number'
                prec = x
                x = precsAndChildren[i++]
            parser = lift x
            parser.addListener @
            @list.push { parser; prec }
        @notify()
        return @

    reindexInternal: ->

        #TODO: attempt to keep alwaysEpsilon property

        # reset indexes
        @indexed  = { }; @unindexed  = [ ];
        @catcodes = { }; @epsilon    = false

        # Reindex, set epsilon property
        # TODO: try to preserve epsilon=='always'
        for entry in @list
            entry.parser.reindex()
            @epsilon='maybe' if entry.parser.epsilon

        # 1st pass: file indexed children under proper indexes
        for entry in @list
            continue unless (catcodes = entry.parser.catcodes)
            for cc of catcodes
                (@indexed[cc] ?= [ ]).push entry
                @catcodes[cc] = true

        # 2nd pass: file unindexed children everywhere
        for entry in @list
            continue if entry.parser.catcodes
            L.log 'algo', "#{@} loses catcode because of #{entry.parser}!"
            @catcode = false
            for _, x of @indexed
                x.push entry
            @unindexed.push entry

        # 3rd pass: sort every list by precedence
        sortByDecreasingPrecedence = (a,b) -> a.prec<b.prec
        for _, x of @indexed
            x.sort sortByDecreasingPrecedence
        @unindexed.sort sortByDecreasingPrecedence

    parseInternal: (lx, prec=0) ->
        util.print "parsing choice at prec #{prec}\n"
        nextTokenCatcode = lx.peek().getCatcode()
        entries = @indexed[nextTokenCatcode] ? @unindexed
        for entry, i in entries
            break if entry.prec < prec
            break if entry.assoc=='right' and entry.prec == prec
            if i>0
                L.log 'algo', "#{@} didn't succeed with first candidate #{
                    entries[0].parser} on #{lx.peek()}, trying with #{entry.parser}"
            L.log 'choice',
                "trying choice #{i+1}/#{entries.length} #{entry.parser} of prec #{entry.prec}"
            result = entry.parser.parse lx
            return result unless result==fail
        return fail

    toString: ->
        return @name ? "Choice(#{(x.parser for x in @list).join ' | '})"

#-------------------------------------------------------------------------------
# TODO: need to create proper messages
#-------------------------------------------------------------------------------
exports.maybe = (x...) -> new Maybe x...
exports.Maybe = class Maybe extends Parser
    typename: 'Maybe'

    constructor: (parser, defaultval) ->
        super
        @parser   = lift parser
        @default  = defaultval ? false
        @parser.addListener @

    reindexInternal: ->
        @parser.reindex()
        @epsilon  = if @parser.epsilon=='always' then 'always' else 'maybe'
        @catcodes = false # always succeeds -> all catcodes are acceptable

    parseInternal: (lx) ->
        result = @parser.parse(lx)
        return (if result==fail then @default else result)

    toString: -> @name ? "Maybe(#{@parser})"

#-------------------------------------------------------------------------------
# Common usage pattern for gg.maybe()
#-------------------------------------------------------------------------------
exports.if = (trigger, primary, whenNotTriggered) ->
    exports.maybe(
        exports.sequence(
            trigger, primary
        ).setBuilder(1),
        whenNotTriggered
    )


#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
exports.list = (x...) -> new List x...
exports.List = class List extends Parser
    constructor: (primary, separator, @canBeEmpty) ->
        super
        @primary    = lift primary
        @separator  = lift separator if separator?
        # TODO: is it a good thing to let it propagate catcodes?
        @catcodes       = @primary.catcodes
        @primary.addListener @
        @dirty = true

    reindexInternal: ->
        @primary.reindex()
        @epsilon = if @canBeEmpty then 'maybe' else @primary.epsilon
        @catcodes = @primary.catcodes # can be false
        if @primary.epsilon
            if @catcodes and @separator?.catcodes
                @catcodes = @catcodes.concat @separator.catcodes
            else @catcodes = false

    parseInternal: (lx) ->
        results = [ ]
        loop
            p = @primary.parse(lx)
            if p==fail then break
            results.push p
            if @separator? and @separator.parse(lx)==fail then break

        return fail if not @canBeEmpty and results.length==0
        return results

    toString: ->
        @name ? if @separator then "List(#{@primary}, #{@separator})" else "List(#{@primary})"

#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
exports.wrap = wrap = (x...) -> new Wrap x...
exports.Wrap = class Wrap extends Parser
    constructor: (parser, @xtraArgs...) ->
        super
        @setParser parser if parser

    notify: -> @catcodes = @parser.catcodes; super

    parseInternal: (lx) -> return @parser.parse lx, @xtraArgs...

    setParser: (parser) ->
        @parser = lift parser
        @parser.addListener @
        @notify()
        return @

    reindexInternal: ->
        @parser.reindex()
        for field in ['catcodes', 'epsilon']
            @[field] = @parser[field]

    toString: ->
        if @name then @name else
            args = [ @parser, @xtraArgs...]
            @name ? if @xtraArgs then "Wrap(#{args.join ', '})"

#-------------------------------------------------------------------------------
# Expression parser generator.
#
# An expression parser allows to combine primary elements with prefix,
# suffix and infix operators. The two formers are unary (they accept one
# subexpression exactly), the latter is binary or n-ary.
# All operators have a precedence attached to them. The higher the precedence,
# the tighter it binds to subexpressions, in case several parse trees would
# have been possible for a given entry.
#
# In addition to precedence, binary operators have associativity,
# which is one of:
# * 'none':  an ambiguous expression such as A+B+C is illegal
# * 'left':  A+B+C is interpreted as (A+B)+C
# * 'right': A+B+C is interpreted as A+(B+C)
# * 'flat':  the operator is n-ary rather than binary, A+B+C is interpreted
#            as +(A, B, C).
#
# @infix, @prefix and @suffix are structures with fields:
#
# * 'indexed' which associate a key to a list of records,
#   sorted by decreasing precedence,
#   with fields 'parser', 'prec', 'builder' (plus 'assoc' for @infix).
#
# * 'unindexed' which contains the list of keyless parsers, sorted by
#   decreasing precedence.
#
# * 'list' where each parser record appears exactly once, for easy reindexing.
#
# TODO: transformers should be applied on all intermediate sub-expressions.
# TODO: marge sets and findParser with Choice, by adding a prec parameter
# to Choice.parse.
#-------------------------------------------------------------------------------
exports.expr = (x...) -> new Expr x...
exports.Expr = class Expr extends Parser
    typename: "Expr"

    constructor: (primary) ->
        super
        @setPrimary primary if primary?
        @prefix   = choice()
        #@infix    = choice()
        @suffix   = choice()
        @catcodes = { }
        @pstore   = { }
        @prefix.addListener @

    # TODO: support key update if expression parsers eventually support catcodes.
    setPrimary: (primary) ->
        @primary = lift primary
        @primary.addListener @
        @notify()
        return @

    reindexInternal: ->
        @prefix.reindex()
        @primary.reindex()
        if @primary.epsilon
            @error "Cannot build an expression parser around epsilon primary parser"
        if @primary?.catcodes and @prefix.catcodes
            @catcodes = { }
            (@catcodes[cc]=true) for cc of p.catcodes for p in [@primary,@prefix]
        else @keys = false

    # TODO add assoc support; need some modification in choice::add()
    # TODO handle flat infix ops
    addPrefix: (x) ->
        @error "missing builder" unless x.builder
        x.parser = lift(x.parser)
        wp = wrap(x.parser).setBuilder((r) -> [x, r])
        @prefix.add x.prec, wp

    addInfix:  (x) ->
        @error "missing builder" unless x.builder
        x.assoc ?= 'left'
        x.parser = lift(x.parser)
        x.kind = 'infix'
        wp = wrap(x.parser).setBuilder((r) -> [x, r])
        @suffix.add x.prec, wp

    addSuffix: (x) ->
        @error "missing builder" unless x.builder
        x.parser = lift(x.parser)
        x.kind = 'suffix'
        wp = wrap(x.parser).setBuilder((r) -> [x, r])
        @suffix.add x.prec, wp

    parseInternal: (lx, prec=0) ->
        L.logindent 'expr', "parsing starts at precedence #{prec}"
        e = @parsePrefix lx, prec
        if e is fail
            L.logdedent 'expr', "parsing failed"
            return fail
        while e2 isnt fail
            e2 = @parseSuffix lx, e, prec
            if e2 is fail then break
            else e=e2
        L.logdedent 'expr', "parsing done at prec #{prec}, e=#{e}"
        return e

    parsePrefix: (lx, prec) ->
        L.log 'expr', "prefix at prec #{prec}?"
        [ p, op ] = @prefix.parse lx, prec
        if p
            e = @parse lx, p.prec
            return @partialBuild p, op, e
        else
            L.log 'expr', "no prefix at prec #{prec}; primary, then."
            return @primary.parse lx, prec

    parseSuffix: (lx, e, prec) ->
        L.log 'expr', "infix/suffix at prec #{prec}?"
        [ p, op ] = @suffix.parse lx, prec
        return fail unless p
        L.log 'expr', "#{p.kind} op #{op} found at prec #{prec}"
        if p.kind is 'infix'
            p_prec = p.prec
            if p.assoc=='left' then p_prec++
            else if p.assoc=='flat'
                @error "flat infix operators not implemented"
            e2 = @parse lx, p_prec
            if e2 is fail
                # TODO: undo & return fail if e2 fails?
                @error "parsing error after infix operator #{op}"
            return @partialBuild p, e, op, e2
        else # p.kind is 'suffix'
            return @partialBuild p, e, op

    # TODO: add transformers
    partialBuild: (p, args...) ->
        r = p.builder args...
        L.log 'expr', "partialBuid(#{args.join ', '}) = #{r}"
        return r

    toString: -> @name ? "Expr(#{@primary}...)"

# Only succeed if the next token is preceded by some spacing.
class Space extends Parser
    constructor: -> super; @dirty=false
    typename: "Space"
    epsilon:  'always'
    catcodes: false
    parseInternal: (lx) -> return (if lx.peek().s then true else fail)
exports.space = new Space()

# Only succeed if the next token is NOT preceded by some spacing.
class NoSpace extends Parser
    constructor: -> super; @dirty=false
    typename: "NoSpace"
    alwaysEpsilon: true
    epsilon:  'always'
    catcodes: false
    parseInternal: (lx) -> return (if lx.peek().s then fail else true)
exports.noSpace = new NoSpace()

# Neutral element: always succeed without consuming any token.
class One extends Parser
    constructor: -> super; @dirty=false
    typename: "One"
    epsilon: 'always'
    catcodes: false
    parseInternal: -> null
exports.one = new One()

# Absorbing element: always fail
exports.zero = lift -> fail

exports.named = (name, parser) ->
    parser.name = name
    return parser