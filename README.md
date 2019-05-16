php-munge
=========

Warning: this project is very much experimental and should not be used by anyone.

This is an extremely early-days attempt to build static analysis tooling for PHP in Haskell. At this point it consists of just a very buggy parser and there are some serious barriers to moving forward. If that interests you, this readme includes more detail about what's here, what's broken, and what still needs to be done.

The eventual design of this library will/should handle three different representations of PHP code:

* A serialized textual representation (what we write in the IDE)
* A concrete syntax tree, corresponding roughly to the [official grammar specification](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md) with some adjustments to be described later
* An annotated abstract syntax tree, corresponding to the [official AST specification](https://wiki.php.net/rfc/abstract_syntax_tree).

The goal is to allow ourselves to parse the serial representation to an AST and render it back in a way that perfectly preserves whitespace, and do this flexibly enough to easily respond to changes to the language as PHP evolves. From there we can think about things like static analysis and linting and running the code in a virtual machine. But step 1 is parsing to an AST.

What we have right now is a buggy parser that constructs CSTs. (Conversion to the AST is not implemented at all, though that part is easier (knock on wood) and I have ideas for how to do it.) The parser is implemented using the Alex/Happy lexer and parser generators -- these are the Haskell variants of lex/yacc. This project is my first time using a parser generator rather than parser combinators; I built a first version of this project with parser combinators, but it turns out the PHP grammar has the assumption of left-recursive grammar productions baked in pretty deeply, making parser combinators awkward to work with. This is not surprising since the 'grammar' of PHP is essentially 'whatever this 25 year old yacc file accepts'.

The lexer description is php-parse/src/PHP/Parse/Lexer.x, and the parser description is in php-parse/grammar/Parser.y. Both lexer and parser use the monadic features of Alex/Happy (this is necessary because PHP is not context-free). The parser is complete in the sense that every production in the spec grammar is accounted for, but test coverage is lacking and there are known bugs.

Each production rule in the grammar has its own type in our representation of the CST. This has a few consequences; the CST is very sensitive to type errors, which is good, but it is also extremely verbose, which is bad (though consumers of the library don't really feel that pain). This is a design decision I made having no experience with parser generators, so it is not necessarily optimal and is subject to revision in the future. It may make more sense to parse directly to the AST, for example -- this is a 'figure it out as we go' situation.

There is an incomplete test suite with two kinds of tests:

* Recognizer tests are handwritten files consisting of come specific concrete syntax that is either well-formed or not. We run the corresponding top-level parser on the file and make sure the outcome is expected. These tests are important for checking specific syntax examples.
* Roundtrip tests generate an arbitrary CST, serialize it, and then parse, making sure the original and parsed CSTs are equal. These are generative tests, meaning we run the tests on hundreds or thousands of test cases using QuickCheck. These tests are important for finding edge cases and weeding out implicit assumptions.

Before working on the AST conversion I'd like to get parsing to the CST closer to 'correct'. This is hindered by very slow compile times, which slows down the usual Haskell iteration cycle.



Trying it Out
-------------

If you want to play with the code, the best way is to have [stack](https://docs.haskellstack.org/en/stable/README/) installed and invoke `make` from the project root directory. This generates the parser and builds the code.

I strongly suggest running the test suite with `stack test --fast` (the make command uses the `--fast` flag, which speeds up compilation a bit, and running the tests without this triggers a recompile).



Known Bugs
----------

* Compilation is EXCRUCIATINGLY slow, and the happy-generated parser module is the main culprit (though the enormous ConcreteSyntax module is bad too). On my machine it takes 20+ minutes to compile this module. Hacking on the parser is not practical while this situation stands. (But once compiled, the parser itself is zippy.)
* The parser has 400+ shift/reduce conflicts (bad) and 25+ reduce/reduce conflicts (really bad). Working on refactoring the grammar to eliminate these, but this does move us away from the official spec. That is not prima facie a problem for this project (unlike the official grammar, which tries very hard to agree with the letter of the official parser) but does require care and testing.
* Parsing Heredocs and Nowdocs is broken, and this fix will be complicated. The basic problem is that these constructs are badly context dependent, requiring us to thread state between the lexer and parser.
* Many of the quickcheck test case generators are broken. The fix for this is not terribly complicated, but is tedious and time consuming. The payoff for fixing this is pretty huge though -- we can generate arbitrary well-formed code samples, which is great for generative testing.
