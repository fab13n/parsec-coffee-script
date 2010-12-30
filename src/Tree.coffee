################################################################################
# tree: temporary hack to generate AST-like stuff without actually linking
# with the CS backend compiler.
################################################################################

exports.tree = (x...) -> new Tree x...

equal = (a,b) ->
    if a instanceof Array
        return false unless b instanceof Array
        # ignore null elements on the right
        if a.length>b.length then [a, b]=[b, a]
        if a.length<b.length
            for i in [a.length...b.length]
                return false if b.length?
        # recursion on each element pair
        for i in [0...a.length]
            return false unless equal(a[i], b[i])
        return true
    else if a instanceof Tree
        return a.tag==b.tag and equal(a.children, b.children)
    else if not a? and not b?
        return true # don't make a difference between null/undefined / length equality isnt compatible with this
    else if not a? or not b?
        return false
    else unless a instanceof Object or b instanceof Object
        return a.toString() == b.toString() # settle string/number type mismatches
    else
        throw new Error "invalid object in tree"

exports.equal=equal

class Tree
    constructor: (@tag, @children...) ->
    toString: -> "`#{@tag}(#{@children.join ', '})"
    toIndentedString: -> toIndentedString @
    equal: (a) -> equal(@, a)

# Print a tree with a readable indentation.
exports.toIndentedString = toIndentedString = (x) ->
    rec = (x, b, i) ->
        if x instanceof Tree
            b.push '`'+x.tag
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
                b.push ",\n  "+i if n<nChildren-1
            b.push " ]"
        else if typeof x is 'string'
            b.push '"'+x+'"'
        else if x?
            b.push x.toString()
        else
            b.push "undefined"
    b = [ ]
    rec(x, b, '')
    return b.join("")

