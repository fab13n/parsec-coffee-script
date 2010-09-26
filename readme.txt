                          Parsec Coffee-Script
                          ===================
                          <metalua@gmail.com>

About.
------

Parsec Coffee Script (PCS) is a parser / compiler for the
coffee-script language, based on parser combinators. The project's aim
is to add static metaprogramming (i.e. macros + syntax extensibility)
to Coffee Script (CS), similar to how Metalua adds such features to
Lua. The resulting compiler, once merged with the official compiler,
should be usable as a drop-in replacement for it.


Roadmap.
--------

The project is decomposed into several steps:

1. Development of the Lexer. Compared to the official CS lexer, it
   must be single pass: no rewriting stage, direct handling of string
   and regex interpolation.

2. Development of the generic parsec library, GrammarGenerator
   (GG). It doesn't need to have a compact/fluid API yet: the final
   API will be based on a static DSL, once PCS is bootstrapped and
   able to define it.

3. Implementation of the coffee-script grammar with GG.

4. Tying the parser front-end to CS' back-end: first working
   PCS-based compiler. At this stage, a merge with the main
   Coffee-Script project will be needed.

5. First implementation of splice (the fundamental operator of static
   metaprogramming).

6. Development of a static DSL, making the definition of GG grammars
   terser and more readable. At this stage, GG grammars are expected
   to be more readable than Bison-inspired grammar definitions.

7. Proper splice, lift (inverse operator of splice) + quasi-quotes

8. Focus on GG usability: good error messages on syntactically
   incorrect inputs, error recovery for multiple errors detection,
   automated bookkeeping of source/Abstract Syntax Tree (AST)
   correspondence.

9. To be determined. A big issue with Metalua was that Lua's variable
   scoping semantics was tricky to combine with usable hygienic
   macros; fixing this in PCS would be a major achievement.

   Another area worthy of investigation would be to determine how much
   of the back-end could be replaced by macros. This would lead to a
   distinction between a core CS, and some more superficial
   extensions. Good candidates for macro-based implementation include
   pattern matching, list comprehensions...


Current state.
--------------

Most of the Lexer is implemented, as well as first-stage GG. The CS
grammar definition is in progress.


A word about static MP and parsec.
----------------------------------

Static metaprogramming allows to execute arbitrary code during
compilation, and to splice user-program-generated AST fragments into
the compiler-generated AST.

Parser combinators are a way to define grammars, especially suitable
to functional languages. Parsers are regular functions, which take
token streams and return AST. Parsers can be defined from scratch, as
regular functions; but the specificity of the parsec approach is the
use of combinators: functions which take simple parsers as arguments,
and return a more complex parser as a result.

Benefits of the parsec approach: 

- In addition to standard combinators (sequence, choice,
  repetition...), some more specific parsers can be defined,
  e.g. expression generators properly handling prefix, infix and
  suffix operators, precedence, associativity, etc. Ad-hoc support for
  common combinators can be backed into Bison-like generators, but
  can't be tailored unless one considers patching the compiler.

- Since parsers are first-class objects (functions), unusual
  operations can be supported through specific functions. In a Bison +
  host language 2-staged approach, there's a big impedance mismatch
  when one wants to mess with the parsing stages in ways not intended
  by Bison; if the parser merely combines plain functions, special
  actions can be easily taken at any stage.

- Most importantly, parsecs can be modified dynamically. Therefore,
  some code in a splice can extend the grammar being recognized in the
  middle of a file being parsed. This allows a file to define and/or
  load grammar extensions seamlessly, reclaiming most of Lisp's
  flexibility, without the constraints Lisp-style macros put on
  superficial syntax.
