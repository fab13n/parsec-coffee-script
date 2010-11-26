################################################################################
# tree: temporary hack to generate AST-like stuff without actually linking
# with the CS backend compiler.
################################################################################

exports.tree = (x...) -> new Tree x...

class Tree
    constructor: (@tag, @children...) ->
    toString: -> "`#{@tag}(#{@children.join ', '})"
    toIndentedString: -> toIndentedString @

# Print a tree with a readable indentation.
exports.toIndentedString = toIndentedString = (x) ->
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
                b.push ",\n  "+i if n<nChildren-1
            b.push " ]"
        else if x?
            b.push x.toString()
        else
            b.push "undefined"
    b = [ ]
    rec(x, b, '')
    return b.join("")
