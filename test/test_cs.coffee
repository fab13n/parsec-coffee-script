cs = require "./../src/cs"
gg = require "./../src/GrammarGenerator"


src = { }

src.id = "foo"

src.fail_keyword = "if"

src.call = """
    foo()
    foo(bar)
    foo(bar, bar)
    foo bar
    foo bar, bar
    foo foo bar, bar
"""

src.fail_call = "foo (1,2,3)"

src.splat = """
    foo...
    foo bar...
"""

src.lambda = """
    (x) -> x
    (x, y) -> x+y
    -> meh
"""

src.super = """
    super
    super 1, 2, 3
    super(1,2,3)
"""

src.fail_super = "super (1,2)"

src.splat = """
    f x...
    (x...) -> y
"""

src.fail_splat = """
    x...
"""

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
"""

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