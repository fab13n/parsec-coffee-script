cs = require "./../src/cs"
gg = require "./../src/GrammarGenerator"


src = { }

# basic identifier
src.id = "foo"

# Don't accept keywords as id
src.fail_id = "if"


# function calls
src.calls = """
    foo()
    foo(bar)
    foo(bar, bar)
    foo bar
    foo bar, bar
    foo foo bar, bar
"""

# no space before function args
src.fail_call = "foo (1,2,3)"

# anonymous functions
src.lambdas = """
    (x) -> x
    (x, y) -> x+y
    -> meh
    (x) -> (y) -> curry
"""

# super invocations
src.super = """
    super
    super 1, 2, 3
    super(1,2,3)
"""

# as for functions, no space before args in parentheses
src.fail_super = "super (1,2)"

src.splat = """
    f x...
    (x...) -> y
"""

# Splats not accepted except as args or params
src.fail_splat = """
    x...
"""

# at-sign
src.at = """
    foo @
    @foo
    @
    @foo(a, @b, @c)
    @foo a, @b, @c
    @[1]
"""

src.fail_at = """
    @@
"""

src.accessor = """
    x[1]
    x[1][2][3]
    x.y
    x.class
    x[1..10]
    x[1...10]
"""

src.array = """
    x( [2])
"""

# multiple values -> index forbidden
src.fail_array = """
    x[1,2]
"""

# This is accepted because it's a list arg, not an index
# TODO: doesn't work without parentheses, indentation handling also broken.
src.not_an_accessor = """
    x( [a,b,c])
"""

src.indented_array = """
    [a, b, c]
    [d, e,
        f]
    [g,
    h,
    i]
    [j, k,
        l, m,
    n, o]
    [p, q,]
    [r,
    ]
"""

# Pass all tests in sequence
for name, x of src
    print "\n***** Test #{name} *****\n"
    t = cs.parse x
    fail = t==gg.fail
    if name.match /^fail_/
        if not fail then throw new Error "Test #{name} should have failed"
        else print "\nCompilation of #{name} failed, as expected\n"
    else
        if fail then throw new Error ("Failure on src.#{name}")
        else print "\n#{name} input:\n#{x}\n\n#{name} result:\n#{cs.toIndentedString t}\n"

print "\nTest passed\n"

#TODO: src finishing with a comment