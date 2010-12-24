# Grammar generator
#

enableLogs = false
if enableLogs then log = print else log = ->

log2=print

this.exports = this unless process?

fail = exports.fail = ["fail"]

pid=1

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
    return x if x instanceof Parser
    return new Sequence(x...) if x instanceof Array
    return new LiftedFunction(x) if x instanceof Function
    return     keyword(x) if typeof x == 'string'
    return     one  if x == 1
    return     zero if x == 0
    throw  new Error "null argument to parser" unless x
    throw  new Error "Parser expected, got #{x.toString()}"

#-------------------------------------------------------------------------------
# Common ancestor for all parsers.
#
# Internal structure:
#
# keys: Optional set of token keys with which this parser might start.
# If there is a set, then it must be exhaustive:
#
#  * it is always OK for a parser to have @keys=false, although it might
#    make it inefficient, especially when used in a Choice combinator;
#
#  * it is OK for a parser to declare, say, "keyword-foo" as a key, but
#    to fail with some token streams starting with keyword "foo";
#
#  * but it is an error for a parser to have a @keys set that doesn't
#    include "keyword-foo" if there are some token streams starting with
#    keyword "foo" that it might parse successfully.
#
#  * as a corollary, a parser with an empty set of keys (as opposed to
#    no set at all, @keys==false), promizes never to succeed parsing
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
callNest = 0
exports.Parser = class Parser
    typename: "Parser"

    constructor: ->
        @builder      ?= (x) -> x
        @transformers ?= [ ]
        @keys         ?= false
        @listeners    ?= [ ]
        @backtrack    ?= true
        @id           ?= pid++
        @dirty        ?= false

    # Run the parser on the token stream `lx', consumming tokens out of it
    # if applicable. Return the object `fail' and leaves the token stream
    # unchanged if parsing fails.
    call: (args...) ->

        callNest++
        log2("|  ") for _ in [0..callNest]
        log2("? #{@toShortString(80)}\n")

        if @dirty then @reindex()

        x = @parse(args...)

        if x==fail
            log2("|  ") for _ in [0..callNest]
            log2("- #{@toShortString()} failed.\n")
            callNest--
            return fail
        else
            if @builder? then x = @builder x
            (x = t(x)) for t in @transformers
            log2("|  ") for _ in [0..callNest]
            log2("+ #{@toShortString()} succeeded, returned '#{x}'.\n")
            callNest--
            return x

    # Internal parsing method: return either a result or `fail', by consumming
    # tokens from lx.
    # This is a protected method which must not be called from outside: use
    # the wrapping method @call(lx) instead.
    parse: (lx) -> @error "Invalid parser"

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
        @dirty=false
        print "<reindexed '#{@toShortString()}', keys = #{@keys2string()}>\n"
        return @

    # DEPRECATED?
    setBacktrack: (x) -> @backtrack = (if x? then x else true); return @

    # When a change is made to this parser, notify all parsers who
    # registered for update notifications.
    # Notification procedure:
    # - a parent parser P's keys depend on its child parser C to determine its
    #   keys, and a change in C's keys might cause a change in P's keys.
    # - P informs C that it needs to be notified about keys changes, by
    #   calling C.addListener(P).
    # - C is subjected to an operation which changes its keys. It recomputes
    #   its own keys, and notifies it listeners, including P.
    # - P receives the notification, updates its keys in its implementation
    #   of @notify; if its own keys have changed, it propagates the notification
    #   through a final supernotify().
    #
    # TODO: delay notification until the first parsing occurs, to avoid
    #       useless multiple notifications. Use a "dirty" flag instead,
    #       and perform a check in @call
    notify: ->
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

    keys2string: ->
        if @keys
            "{ " + ("'#{k.replace /^keyword\-/, '!'}'" for k of @keys).join(", ") + " }"
        else
            "NOKEYS"


#-------------------------------------------------------------------------------
# Wrap a function into a parser, so that it respects the parser API:
# (called via method .parse(lx), supporting transformers list).
#-------------------------------------------------------------------------------
exports.liftedFunction = (x...) -> new LiftedFunction x...
exports.LiftedFunction = class LiftedFunction extends Parser
    # field f: function to be applied
    typename:    'Function'
    constructor: (@f) -> super
    parse:       (lx) -> return @f(lx)


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

        @keys = { }
        if valueKeyed?
            if @t=='keyword' then @name='!'+values.join '-'
            else @name = @t + '-' + values.join '-'
            (@keys[@t+'-'+v] = true) for v in values
        else
            @name = @t
            @keys[@t] = true

    parse: (lx) ->
        tok = lx.peek()
        log "is #{tok} a #{@}? "
        if tok.t != @t then log "no!\n"; return fail
        if @values? and not @values[tok.v] then log "no!\n"; return fail
        log "yes!\n"
        return lx.next().v or true

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
    constructor: -> super
    parse: (lx) ->
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
        @children = lift child for child in children
        @dirty=true

    firstNonEpsilonChild: ->
        for c in @children
            return c unless c instanceof EpsilonParser
        return false

    reindex: ->
        return @ unless @dirty
        fnec = @firstNonEpsilonChild()
        if fnec
            fnec.reindex()
            @keys = fnec.keys
        else
            @keys=false # TODO: maybe sometimes { } ?
        return super

    parse: (lx) ->
        result   = []
        bookmark = lx.save()
        for child, i in @children
            log "Sequence child ##{i}, token=#{lx.peek()}, parser=#{child.toShortString()}...\n"
            x = child.call(lx)
            if x == fail
                if @backtrack or i==0
                    log ">>>> BACKTRACKING FROM #{lx.peek()} TO "
                    lx.restore bookmark
                    log "#{lx.peek()} in #{@toShortString()} <<<<<\n"
                    return fail
                else
                    @error "failed on element ##{i}"
            else result.push x
            #rl=result.length; log "result of child #{rl}: #{result[rl-1]}\n"
        return result

    toString: -> @name ? "Sequence(#{@children.join ', '})"


#-------------------------------------------------------------------------------
# Choose between alternative parsers, according to the first token's key.
#
# Children parsers are sorted by keys.
#
#-------------------------------------------------------------------------------
exports.choice = (x...) -> new Choice x...
exports.Choice = class Choice extends Parser

    # field indexed:    parsers to choose from, indexed by key.
    #                   key -> list of children, sorted by decreasing precedence.
    # field indexedP:   children precedences, indexed by key then order.
    #                   key -> list of precedences, the n-th entry is the precedence
    #                   of the n-th parser in the corresponding parser list.
    # field unindexed:  key-less children, sorted by decreasing order.
    # field unindexedP: precedences of key-less children.
    # field default:    the optional keyless parser TODO UPDATE
    # field unused:     parsers which are currently never used (they have zero keys)
    #                   They might get new keys later, and notify the Choice combinator,
    #                   which will then move them in @indexed or @unindexed.
    # field unusedP:    precedence of unused children.
    #
    # TODO: structurate as Expr: by putting parsers and parsers metadata in s single record,
    #       by appending unindexed parser to every index,
    #       by keeping a list.
    #       if precedence is kept in list, it might be removed from indexes.

    constructor: (children...) ->
        super
        @indexed    = { }
        @unindexed  = [ ]
        @keys       = { }
        @indexedP   = { }
        @unindexedP = [ ]
        @unused     = [ ]
        @unusedP    = [ ]
        @add children... if children.length>0

    # children are not put directly in the correct index. Instead,
    # the whole Choice parser is marked as dirty, so that it will be reindexed
    # correctly the first time it's used.
    add: (prec, children...) ->
        if typeof prec != 'number'
            children.unshift prec
            prec = 50

        i=0
        len=children.length
        while i<len
            x = children[i++]
            if typeof i == 'number'
                prec = x
                x = children[i++]
            child = lift x
            @unused.push child
            @unusedP.push prec--
            child.addListener @

        @notify() if children.length>0
        return @

    # Take all the children indexes apart, and reinsert every child in
    # the correct indexes, after having triggered its own reindexing.
    # Also reorganize the precedence lists correctly, in case other
    # reindexings are triggered later.
    reindex: ->

        return @ unless @dirty

        # insert a child and its precedence in the correct lists,
        # at places which preserve the property that precedence lists are
        # sorted by decreasing precedence.
        insertWithPrec = (list, listP, x, p) ->
            i=0
            length=list.length
            i++ until listP[i] >= p or i>=length
            for j in [i ... length]
                return if list[j] == x # duplicate
                break if listP[j] != p # remaining ones have higher precedence
            listP.splice i, 0, p
            list.splice  i, 0, x

        # Reconstitute the lists of all childrens and precedences
        allChildren = @unused.concat @unindexed
        allPrecs    = @unusedP.concat @unindexedP
        for key, children of @indexed
            precs = @indexedP[key]
            for i in [0...children.length]
                allChildren.push children[i]
                allPrecs.push precs[i]

        # reset indexes
        @indexed  = { }; @unindexed  = [ ]; @unused  = [ ]; @keys = { }
        @indexedP = { }; @unindexedP = [ ]; @unusedP = [ ];

        # reinsert in appropriate index
        for i in [0 ... allChildren.length]
            child = allChildren[i]
            prec  = allPrecs[i]
            child.reindex()
            unless child.keys
                insertWithPrec @unindexed, @unindexedP, child, prec
            else
                for key of child.keys
                    hasAtLeastOneKey = true # TODO: is there an emptiness test for objects?
                    parsers = (@indexed[key]  ?= [ ])
                    precs   = (@indexedP[key] ?= [ ])
                    insertWithPrec parsers, precs, child, prec
                    @keys[key] = true if @keys
                unless hasAtLeastOneKey
                    insertWithPrec @unused, @unusedP, child, prec
        return super

    parse: (lx) ->
        log "parse Choice on #{lx.peek()}\n"
        nextTokenKey = lx.peek().getKey()
        parsers = @indexed[nextTokenKey]
        if parsers then for p in parsers
            result = p.call lx
            return result unless result==fail
        for p in @unindexed
            result = p.call lx
            return result unless result==fail
        return fail

    toString: ->
        return @name if @name
        allChildren = [ ].concat(@unindexed).concat(@unused)
        for k, children of @indexed
            allChildren = allChildren.concat(children)
        return "Choice(#{allChildren.join ' | '})"

#-------------------------------------------------------------------------------
# TODO: need to create proper messages
#-------------------------------------------------------------------------------
exports.maybe = (x...) -> new Maybe x...
exports.Maybe = class Maybe extends Parser
    typename: 'Maybe'

    constructor: (parser) ->
        super
        @parser = lift parser

    parse: (lx) ->
        result = @parser.call(lx)
        if result==fail then return false else return result

    toString: -> @name ? "Maybe(#{@parser})"

#-------------------------------------------------------------------------------
# TODO: maybe terminators don't make sense anymore
#-------------------------------------------------------------------------------
exports.list = (x...) -> new List x...
exports.List = class List extends Parser
    constructor: (primary, separator, @canBeEmpty) ->
        super
        @primary    = lift primary
        @separator  = lift separator if separator?
        # TODO: is it a good thing to let it propagate keys?
        @keys       = @primary.keys
        @primary.addListener @
        @dirty = true

    reindex: ->
        return @ unless @dirty
        if @canBeEmpty then @keys=false
        else @primary.reindex(); @keys = @primary.keys
        return super

    parse: (lx) ->
        results = [ ]
        loop
            p = @primary.call(lx)
            if p==fail then break
            results.push p
            if @separator? and @separator.call(lx)==fail then break
            log "#{@} again\n"
        log "#{@} done, #{results.length} elements\n" if results.length>0

        return fail if not @canBeEmpty and results.length==0
        return results

    toString: ->
        @name ? if @separator then "List(#{@primary}, #{@separator})" else "List(#{@primary})"

#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
exports.wrap = (x...) -> new Wrap x...
exports.Wrap = class Wrap extends Parser
    constructor: (parser) ->
        super
        @setParser parser if parser

    notify: -> @keys = @parser.keys; super

    parse: (lx) -> log "lx in wrap=#{lx}\n"; return @parser.call lx

    setParser: (parser) ->
        @parser = lift parser
        @parser.addListener @
        @notify()
        return @

    reindex: ->
        return @ unless @dirty
        @parser.reindex()
        @keys = @parser.keys
        return super

    toString: -> @name ? "Wrap(#{@parser})"

#-------------------------------------------------------------------------------
# If(triggerParser, parser, whenNotTriggered):
# when the triggerParser fails, the value in whenNotTriggered
#-------------------------------------------------------------------------------
exports.if = (x...) -> new If x...
exports.If = class If extends Parser
    constructor: (trigger, parser, @whenNotTriggered) ->
        super
        @trigger = lift trigger
        @parser  = lift parser
        @keys    = @trigger.keys
        @trigger.addListener @

    reindex: ->
        return @ unless @dirty
        @trigger.reindex()
        @keys = @trigger.keys
        return super

    parse: (lx) ->
        if @trigger.call(lx) != fail
            bookmark = lx.save()
            result = @parser.call lx
            if result == fail then lx.restore bookmark; return fail
            else return result
        else return @whenNotTriggered

    toString: -> @name ? "If(#{@trigger},  #{@parser}, #{@whenNotTriggered})"

#-------------------------------------------------------------------------------
# TODO: remove?
# Token predicate.
#
# Take a predicate on tokens as a constructor parameter, suceed by consuming
# the next token iff it satisfied that predicate.
#-------------------------------------------------------------------------------
exports.filter = (x...) -> new Filter x...
exports.Filter = class Filter extends Parser
    constructor: (@predicate) -> super
    parse: (lx) ->
        if @predicate(lx.peek()) then return lx.next() else return fail

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
# * indexed which associate a key to a
#   record with fields 'parser', 'prec', 'builder' (and 'assoc' for @infix).
# * unindexed which contains the optional keyless parser.
# * list where each parser record appears exactly once, for easy reindexing.
#
# TODO: transformers should be applied on all intermediate sub-expressions.
#-------------------------------------------------------------------------------
exports.expr = (x...) -> new Expr x...
exports.Expr = class Expr extends Parser
    typename: "Expr"

    constructor: (primary) ->
        super
        @setPrimary primary if primary?
        @prefix  = { indexed: { }, unindexed: [ ], list: [ ] }
        @infix   = { indexed: { }, unindexed: [ ], list: [ ] }
        @suffix  = { indexed: { }, unindexed: [ ], list: [ ] }
        @keys    = { }

    # TODO: support key update if expression parsers eventually support keys.
    setPrimary: (primary) ->
        @primary = lift primary
        @primary.addListener @
        @notify()
        return @

    reindex: ->
        return @ unless @dirty
        @primary.reindex()

        for setname in ['prefix', 'infix', 'suffix']
            oldset = @[setname]
            newset = { indexed: { }, unindexed: [ ], list: oldset.list }
            for p in oldset.list
                p.parser.reindex()
                keys = p.parser.keys
                if not keys
                    newset.unindexed.push p
                else for k of keys
                    (newset.indexed[k] ?= [ ]).push p
            @[setname] = newset

            precSort = (a,b) -> a.prec>b.prec
            newset.unindexed.sort precSort
            if newset.unindexed.length>0 then for k, list of newset.indexed
                newset.indexed[k] = list.concat newset.unindexed
            for k, list of newset.indexed
                list.sort precSort
            @[setname] = newset

        if not @primary.keys or @prefix.unindexed.length>0 then @keys=false
        else
            @keys = { }
            (@keys[k] = true) for k of @primary.keys
            (@keys[k] = true) for k of @prefix
        return super

    addPrefix: (x) -> @add 'prefix', x
    addInfix:  (x) -> @add 'infix',  x
    addSuffix: (x) -> @add 'suffix', x

    # add a rule to the expression parser.
    # set: 'prefix', 'infix' or 'suffix'.
    # x: object with fields parser, prec, builder.
    # For infix operators it should also have assoc.
    # prec defaults to 50, assoc defaults to 'left'.
    add: (setname, x)->
        log("adding #{setname} operator #{x}\n")
        set = @[setname]
        x.parser = lift x.parser
        x.prec ?= 50
        x.assoc ?= 'left' if setname == 'infix'
        set.list.push x
        @notify()
        return @

    parse: (lx, prec) ->
        prec ?= 0
        log "parsing starts at precedence #{prec}\n"
        e = @parsePrefix lx, prec
        return fail if e==fail
        again = true
        while again
            again = false
            e2 = @parseSuffix lx, e, prec
            if e2 != fail then e=e2; again=true; log "suffix success\n"
            e2 = @parseInfix  lx, e, prec
            if e2 != fail then e=e2; again=true; log "infix success\n"
        @error "expr fucked up" if e==fail
        log "parsing done, e=#{e}\n"
        return e

    parsePrefix: (lx, prec) ->
        log "prefix\n"
        { p, op } = @findParser @prefix, lx, prec
        log "prefix op candidate: #{op}\n"
        if p
            e = @call lx, p.prec
            return @partialBuild p, op, e
        else
            log "primary, then.\n"
            return @primary.call lx


    parseInfix:  (lx, e, prec) ->
        log "infix\n"
        { p, op } = @findParser @infix, lx, prec
        return fail unless p

        if p.assoc == 'flat'
            operands = [e]
            loop
                operands.push @call lx, p.prec
                break if p.parser.call lx, p.prec == fail
            return @partialBuild p, operands

        else if p.assoc == 'none' and p.prec == prec
            # TODO: I don't really know what i'm doing here
            log "Warning, non-associative operator can't resolve precedence\n"
            return fail

        else
            log "parsed operator #{lx.peek().v} (#{p.prec}) because current precedence was #{prec}\n"
            log "about to parse e2, next is #{lx.peek()}\n"
            e2 = @call lx, p.prec
            # TODO: undo & return fail if e2 fails?
            log "e2=#{e2}\n"
            return @partialBuild p, e, op, e2

    parseSuffix: (lx, e, prec) ->
        log "suffix\n"
        { p, op } = @findParser @suffix, lx, prec
        return fail unless p
        return @partialBuild p, e, op


    findParser: (set, lx, prec) ->
        log("find a parser for #{lx.peek()}\n")
        candidates = set.indexed[lx.peek().getKey()] or set.unindexed
        log("candidates = #{set.indexed}\n")
        if candidates then for p in candidates
            if p.prec<prec or p.assoc=='right' and p.prec==prec then continue
            op = p.parser.call lx
            if op != fail then log("found a candidate #{p}\n"); return { p, op }
        log("nothing found\n")
        return { p:false, op:fail }

    partialBuild: (p, args...) ->
        log "pbuild #{args}, "
        b = p.builder
        if not b then r = args[0]
        else if typeof b == 'number' then r = args[b]
        else r = b(args...)
        log "result = #{r}\n"
        return r

    toString: -> @name ? "Expr(#{@primary}...)"

# Common ancestor of special parsers which don't consume any token.
exports.EpsilonParser = class EpsilonParser extends Parser

# Only succeed if the next token is preceded by some spacing.
class Space extends EpsilonParser
    typename: "Space"
    parse: (lx) -> return (if lx.peek().s then true else fail)
exports.space = new Space()

# Only succeed if the next token is NOT preceded by some spacing.
class NoSpace extends EpsilonParser
    typename: "NoSpace"
    parse: (lx) -> return (if lx.peek().s then fail else true)
exports.noSpace = new NoSpace()

# Neutral element: always succeed without consuming any token.
class One extends EpsilonParser
    typename: "One"
    parse: -> null
exports.one = new One()

# Absorbing element: always fail
exports.zero = lift -> fail

exports.named = (name, parser) ->
    parser.name = name + "/" + parser.id
    return parser