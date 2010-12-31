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
   and regex interpolation. [IN PROGRESS]

2. Development of the generic parsec library, GrammarGenerator
   (GG). It doesn't need to have a compact/fluid API yet: the final
   API will be based on a static DSL, once PCS is bootstrapped and
   able to define it. [DONE]

3. Implementation of the coffee-script parser (PCS) with GG 
   [IN PROGRESS].

4. Defining an Abstract Syntax Tree (AST) format [DONE].

5. Merge with Coffee-Script's compiler backend Node.js

At this stage, PCS will be a fully functional Coffee-Script compiler.

6. Implementation of splice (the fundamental operator of static
   metaprogramming).

7. Development of a static DSL, making the definition of GG grammars
   terser and more readable. At this stage, GG grammars are expected
   to be more readable than Bison-inspired grammar definitions.

8. Lift (inverse operator of splice) + quasi-quotes

9. Focus on GG usability: good error messages on syntactically
   incorrect inputs, error recovery for multiple errors detection,
   automated bookkeeping of source/Abstract Syntax Tree (AST)
   correspondence.

10. To be determined: macro hygiene, experimental language extensions,
    modularisation of Coffee-Script (implement non-core features as
    syntax extensions, in order to simplify the AST->js compiler)...


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
