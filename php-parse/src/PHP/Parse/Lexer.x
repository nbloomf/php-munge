-- This file describes a lexical analyzer for PHP -- a tool that converts strings of unicode symbols into a form that can then be more easily turned into an abstract syntax tree. The analyzer is built using Alex, a Lex-style lexical analyzer generator for the Haskell ecosystem.

-- Before diving into the details it's worth going into some background on lexical analysis and the vagaries of real programming languages. This will help explain why we're doing what we're doing and how we use the facilities Alex provides to work with PHP.

--   (1) Overview of Lexical Analysis
--   (2) Context in PHP
--   (3) Lexical Macros
--   (4) Token Recognizers
--   (5) Lexical State
--   (6) The Parser Monad

-- Two things worth noting up front: first of all, the ultimate goal here is to build tools for doing static analysis on large PHP projects. For this reason our lexer and parser will keep track of whitespace, and we'll be very persnickety about making sure we can reconstruct original source files byte-for-byte from the parsed AST.
--
-- Second, this wouldn't have been possible without the work that many people put into the draft PHP language specification at https://github.com/php/php-langspec/. I am not a PHP expert -- hence the urge to build tools for static analysis in not-PHP. :)

{
module PHP.Parse.Lexer where

import qualified Data.Text.Lazy as T

import PHP.Parse.Loc
import qualified PHP.Parse.LexicalGrammar as Lex
import qualified Data.List as L
}

%wrapper "monadUserState"





-- ============================ --
-- Overview of Lexical Analysis --
-- ============================ --

-- In the broadest and happiest terms a _lexical analyzer_ is a function taking streams of symbols in a _source language_ to streams of symbols in a _target language_ (or to an error). In programming contexts, often the source language is the set of Unicode characters and the target language is a set of abstract tokens, and the purpose of lexical analysis is to convert an expression _from_ a form easy for humans to write _into_ a form easy for computers to parse. Seen in this way there's nothing magical about lexical analyzers -- they are functions that turn lists into other lists, with a type signature something like this:
--
--     lex :: [InputToken] -> Either LexicalError [OutputToken]
--
-- The process of evaluating this function is called _lexing_.

-- (N.b.: A more sophisticated view has `lex` map into a monad capable of dealing with other effects and communicating with later stages of a compiler pipeline, but the simple version gives the right intuition.)

-- _Lexical analyzer generators_ are tools that let us build lexical analyzers in terms of their high-level structure. Generators make some explicit assumptions about what kind of lexical grammar we have, with the tradeoff that implementing such grammars is much easier with the tool than if we do it by hand. There's an analogy here with general purpose programming languages vs. machine code. In our case, Alex is a generator that compiles this document containing a description of a lexical analyzer into a Haskell module which implements it.

-- Alex in particular (as a spiritual descendent of Lex) constructs analyzers of the following form:
--
--   (1) We have a set of regular expressions, called _recognizers_, which represent the streams of input symbols we expect to see at the head of the input stream.
--   (2) Each recognizer has an associated token constructor which takes a matched string and returns a symbol in the target language.
--   (3) As long as any input symbols remain, the recognizers have a contest. We look for the _longest_ prefix of the input stream that matches at least one recognizer. In general more than one recognizer can have a longest match, and so we have a mechanism for choosing a unique winner among them.
--   (4) We remove the matched prefix from the input stream and emit the token for the winning recognizer to the output stream.
--   (5) If at any point input symbols remain but no recognizer matches, we report a lexical error.
--
-- This strategy is a vast simplification of the set of possible lexical analyzers but it has some significant benefits. First it is not too severe a restriction on the design space of lexical grammars; there's a lot we can do, and we're not really interested in building _arbitrary_ lexical analyzers anyway. Second, this strategy is pretty easy to reason about and fits a fairly general mental model of how languages work. And finally, analyzers of this form can be implemented efficiently using very general tools, saving us a lot of effort and making it easier to iterate on grammar designs.

-- To give an example, consider strings of text with two unix style single character escape sequences, "\n" and "\\", where a backslash followed by any other character is considered an error. This grammar might have three recognizers: one matching '\n', one matching '\\', and one matching sequences of characters that do not include any backslashes. The output language has three kinds of tokens, e.g. NEWLINE, BACKSLASH, and LITERAL.

-- Grammars like this one are very special: note that the set of recognizers and their interpretations does not change over the course of lexing. This is not the case for more complicated grammars; as an example consider the "$" character in PHP. If this character is the longest match at some point while lexing a PHP source file, what output token does it correspond to? It could denote that a variable name is coming up. Inside a double quoted string it could mean that a complex expression is coming up. In a nowdoc or outside of PHP tags it could just be a dollar sign. And all three of these possibilities have fundamentally different semantics, which is what we really care about. So the real answer to what "$" means is _it depends_ -- it depends on the _context_.

-- Context dependence like this is probably inevitable once a language becomes expressive enough, and so Alex (again as a spiritual successor to Lex) has a mechanism for dealing with it. As it is built into the tool this mechanism is very simple, but we can build richer context dependence on top. To get into this we'll need to turn our attention more specifically to PHP.





-- ============== --
-- Context in PHP --
-- ============== --

-- Our most basic tool for handling context with Alex is called a _startcode_. In a nutshell Alex allows us to define multiple overlapping sets of recognizers, each one indexed by an integer called a startcode. Then during lexing it keeps track of a current startcode S, only considering recognizers in the set with index S. When a recognizer matches and we generate an output token we can also change the current startcode, altering the set of recognizers that become active for the next token.

-- We will refer to the different startcodes as _lexical modes_. These are our first and most basic tool for dealing with context, and our only means of communicating context to Alex. For instance, single quoted strings in PHP form an especially simple subgrammar which we can isolate using a lexical mode.

-- But keeping track of only the current lexical mode is not enough. To see why, consider the single quoted string example again; if we are in single quoted string mode and see a single quote, what happens next? We're done lexing the single quoted string and now we need to change the mode, ideally back to whatever it was before the string. So we need to keep track of a history of modes.

-- There are a few ways to do this, but I claim the simplest one that does the job is to maintain a _stack_ of modes -- when we enter a new mode, push the old one onto a stack, and when we're done with the current mode, pop it from the stack. One reason to use a stack rather than some more complicated structure is that eventually the output tokens will be further processed into a tree, and the execution trace of interactions with a stack is naturally a tree, making it easier to reason about the interaction between lexing and parsing. We will call this stack of lexical modes the _lexical context_.

-- Even this is not enough to model PHP's lexical grammar -- at least, there's one extra bit of state we can maintain to make lexing PHP a little easier. To motivate it, consider what can happen inside a double quoted string.
--
--   $foo = "Just some text ${ $bar->baz[ 1 + $qux{'a'} ] } omg what was that";
--
-- When we see the first double quote, we enter a double quoted string mode. Great. Then we see a "${", which means we need to start looking for an expression. How long do we look for expression tokens? Well, up to the _balancing_ closed curly brace. And here's problem #1: to lex this properly we need to know which closed curly brace balances. You might complain that closed curly braces cannot appear in an expression -- but in fact they can, becuase '$foo{1}' is a synonym for '$foo[1]', and besides, the same problem exists when lexing array index expressions. So we also need to maintain a stack of semantic delimiters like '{'/'}' and '['/']'. It gets worse, though, becuase even when we reach the balanced closing delimiter, what we really want is to restore the lexical context to what it was before we started lexing the inner context. And here's problem #2: we can't be certain of what the stack of modes looks like by that point.

-- There is hope though. In the double quoted string example, note that inside the inner expression context we don't need to know the details of the lexical context that brought us there; we just need to be able to restore that context when we see a balanced closing delimiter. And this is generally the case; context sensitivity only has a role when an inner context is closed. So we can maintain lexical contexts with the stack of delimiters. Pushing a new semantic delimiter may stash the lexical context, and popping a semantic delimiter restores the stashed context. We'll call this the _delimited context_.

-- Unfortunately due to the way Alex works we have to defer the details of all this state until the end of the module, but to summarize, while lexing we maintain three levels of context:
--
--   (1) The current _lexical mode_, governing which recognizers are active;
--   (2) The current _lexical context_, a stack of modes tracking "how we got here";
--   (3) The current _delimited context_, a stack of outstanding delimiters with (optionally) stashed lexical contexts to be restored on closure.
--
-- It sounds like a lot to carry in our heads, but just think "stack of stacks".





-- ============== --
-- Lexical Macros --
-- ============== --

-- We begin by defining some regex macros; these will simplify the task of writing token recognizers later.

-- First, synonyms for the digit classes.

$bin = [0-1]
$oct = [0-7]
$pos = [1-9]
$dec = [0-9]
$hex = [0-9 a-f A-F]

-- Next, a regex for recognizing _names_. These are used
-- as identifiers in the grammar.

$nameDigit    = [ 0-9 ]
$nameNondigit = [ \_ a-z A-Z \x80-\xff ]

@name         = $nameNondigit [ $nameDigit $nameNondigit ]*

-- Finally, keywords in PHP are not case sensitive.
-- These macros allow us to recognize keywords easily
-- regardless of case.

$A = [aA]
$B = [bB]
$C = [cC]
$D = [dD]
$E = [eE]
$F = [fF]
$G = [gG]
$H = [hH]
$I = [iI]
$J = [jJ]
$K = [kK]
$L = [lL]
$M = [mM]
$N = [nN]
$O = [oO]
$P = [pP]
$Q = [qQ]
$R = [rR]
$S = [sS]
$T = [tT]
$U = [uU]
$V = [vV]
$W = [wW]
$X = [xX]
$Y = [yY]
$Z = [zZ]





-- ================= --
-- Token Recognizers --
-- ================= --

-- We're about to enter the realm of Alex syntax. It's scary at first, but not so bad once we get used to it. Every recognizer has the following form:
--
--   <startcode>  regex  { token constructor }
--
-- where _startcode_ is a list of startcodes on which the recognizer should fire, _regex_ is the regular expression for matching tokens, and _token_constructor_ is a Haskell function which (surprise!) constructs a token after being given some context about the match.
--
-- The token producer function must have this signature:
--
--   (AlexPosn, Char, [Byte], String) -> Int -> Alex Token
--
-- which is kind of a mouthful. To help out with type safety we'll define these token producers using several helper functions which, due to quirks in how Alex works, have to be defined at the end of this module.

tokens :-



-- Text Mode --
---------------

-- _Text Mode_ is what we call anything appearing outside of `<?php ?>` tags. Tokenizing in text mode is pretty straightforward: take any character until we see one of the PHP start tags, and when we see a start tag enter Command Mode.

<0>  [.\n]      { _anychar Lex.CC_Text                      }

<0>  \<\?php    { _phptag  Lex.TG_StartLong  (pushMode cmd) }
<0>  \<\?=      { _phptag  Lex.TG_StartShort (pushMode cmd) }



-- Single Line Comments --
--------------------------

-- Single line comments can only be started from inside command mode, and there are two ways to do so: with '//' and '#'.
--
-- There are two ways to exit: a newline pops us back to command mode, while a closing PHP tag flushes the mode stack and puts us back in Text mode (equivalent to calling pop on the stack until it is empty).
--
-- Inside this mode we match (1) any character other than '?', '\n', and '\r' or (2) any '?' that is either _not_ followed by a '>' (since that would form a closing tag) or is at the end of the stream.

<cmd>  [\/][\/]       { _symbol  Lex.SY_DoubleSlash       (pushMode slc) }
<cmd>  [\#]           { _symbol  Lex.SY_Octothorpe        (pushMode slc) }

<slc>  [\n]           { _newline Lex.NL_LF                popMode        }
<slc>  [\r]           { _newline Lex.NL_CR                popMode        }
<slc>  [\r][\n]       { _newline Lex.NL_CRLF              popMode        }

<slc>  [\?][\>]       { _phptag  Lex.TG_End               flushMode      }

<slc>  ~[\?\r\n]      { _anychar Lex.CC_SingleLineComment                }
<slc>  [\?] / ~[\>]?  { _anychar Lex.CC_SingleLineComment                }



-- Multi Line Comments --
-------------------------

-- Multi line comments can only be started from command mode via '/*', and we can only pop back via '*/'.
--
-- Inside this mode we match (1) any character other than '*' or (2) any '*' that is _not_ followed by '/' (since that would form a closing token).

<cmd>  [\/][\*]     { _symbol  Lex.SY_SlashAst         (pushMode mlc) }
<mlc>  [\*][\/]     { _symbol  Lex.SY_AstSlash         popMode        }

<mlc>  [.\n] # [\*] { _anychar Lex.CC_MultiLineComment                }
<mlc>  [\*] / ~[\/] { _anychar Lex.CC_MultiLineComment                }
<mlc>  [\*] / [\n]  { _anychar Lex.CC_MultiLineComment                }



-- Floating Literals --
-----------------------

<cmd>  [$dec]+\.[$dec]* / [eE] { _string Lex.SC_FractionalDigits (pushMode pow) }
<cmd>  \.[$dec]+        / [eE] { _string Lex.SC_FractionalDigits (pushMode pow) }
<cmd>  [$dec]+          / [eE] { _string Lex.SC_FractionalDigits (pushMode pow) }

<cmd>  [$dec]+\.[$dec]*        { _string Lex.SC_FractionalDigits ok             }
<cmd>  \.[$dec]+               { _string Lex.SC_FractionalDigits ok             }

<pow>  e                       { _prefix Lex.PX_Frac_SmallE      ok             }
<pow>  E                       { _prefix Lex.PX_Frac_LargeE      ok             }

<pow>  \+                      { _symbol Lex.SY_Plus             ok             }
<pow>  \-                      { _symbol Lex.SY_Minus            ok             }

<pow>  [$dec]+                 { _string Lex.SC_ExponentDigits   popMode        }



-- Integer Literals --
----------------------

-- The recognizers for integer literals need to come after those for floating literals; otherwise strings like "123e4" do not lex correctly.

<cmd>  0B             { _prefix Lex.PX_Binary_LargeB (pushMode bin) }
<cmd>  0b             { _prefix Lex.PX_Binary_SmallB (pushMode bin) }
<bin>  [$bin]+        { _string Lex.SC_BinaryDigits  popMode        }

<cmd>  0X             { _prefix Lex.PX_Hex_LargeX    (pushMode hex) }
<cmd>  0x             { _prefix Lex.PX_Hex_SmallX    (pushMode hex) }
<hex>  [$hex]+        { _string Lex.SC_HexDigits     popMode        }

<cmd>  0              { _prefix Lex.PX_Octal         (pushMode oct) }
<oct>  [$oct]+        { _string Lex.SC_OctalDigits   popMode        }

<cmd>  [$pos][$dec]*  { _string Lex.SC_DecimalDigits ok             }



-- Single Quoted Strings --
---------------------------

-- Single quoted strings are entered from command mode via a "'" character. Inside this mode we accept any character _except_ a backslash which is followed by either "'" or "\"; the literals "\'" and "\\" are interpreted as escape sequences. An unescaped "'" pops us back to command mode.

<cmd>  [']           { _symbol  Lex.SY_SingleQuote                    (pushMode sqt) }
<sqt>  [']           { _symbol  Lex.SY_SingleQuote                    popMode        }

<sqt>  [.\n] # ['\\] { _anychar Lex.CC_SingleQuotedString                            }
<sqt>  [\\] / ~['\\] { _anychar Lex.CC_SingleQuotedString                            }
<sqt>  [\\] / [\n]   { _anychar Lex.CC_SingleQuotedString                            }

<sqt>  \\'           { _escaped Lex.ES_SingleQuotedString_SingleQuote                }
<sqt>  \\\\          { _escaped Lex.ES_SingleQuotedString_Backslash                  }



-- Double Quoted Strings --
---------------------------

-- Double quoted strings are quite a bit more complicated. First of all, we enter and exit this mode with a '"'.

<cmd>  ["]      { _symbol Lex.SY_DoubleQuote (pushMode dqt) }
<dqt>  ["]      { _symbol Lex.SY_DoubleQuote popMode        }

-- Within this mode there are a few kinds of escaped characters. There's the usual C-style escape codes:

<dqt>  \\"      { _escaped Lex.ES_DoubleQuotedString_DoubleQuote    }
<dqt>  \\\\     { _escaped Lex.ES_DoubleQuotedString_Backslash      }
<dqt>  \\\$     { _escaped Lex.ES_DoubleQuotedString_Dollar         }
<dqt>  \\e      { _escaped Lex.ES_DoubleQuotedString_Escape         }
<dqt>  \\f      { _escaped Lex.ES_DoubleQuotedString_FormFeed       }
<dqt>  \\n      { _escaped Lex.ES_DoubleQuotedString_LineFeed       }
<dqt>  \\r      { _escaped Lex.ES_DoubleQuotedString_CarriageReturn }
<dqt>  \\t      { _escaped Lex.ES_DoubleQuotedString_HorizontalTab  }
<dqt>  \\v      { _escaped Lex.ES_DoubleQuotedString_VerticalTab    }

-- And octal, hex, and unicode style escape sequences.

<dqt>  \\[$oct]{1,3}   { _string Lex.SC_OctalEscape   ok }
<dqt>  \\[xX][$hex]+   { _string Lex.SC_HexEscape     ok }
<dqt>  \\u\{[$hex]+\}  { _string Lex.SC_UnicodeEscape ok }

-- The complexity of double quoted strings comes from the ability to include _expressions_ in two styles: simple and complex. I have not been able to find an explanation of exactly what this means beyond looking at the code for the PHP interpreter.



-- Simple Expressions
---------------------

-- Every simple expression starts with a variable name, which we can recognize as a '$' followed by the first character of a name.

<dqt>  \$ / $nameNondigit { _symbol Lex.SY_Dollar (pushMode dqs) }

-- Member access expressions are then followed immediately by '->'; we detect an lex this in `dqa` mode. Upon lexing a single name, we want to pop back to dqt mode directly.

<dqs>  @name / \->   { _string Lex.SC_Name         (pushMode dqa)       }

<dqa>  \->           { _symbol Lex.SY_MinusGreater ok                   }
<dqa>  @name         { _string Lex.SC_Name         (popMode >> popMode) }

-- Similarly, array index expressions are followed by '['. We detect and lex this in `dqb` mode. This case is more complicated because the array index expression can itself be a name, a variable name, or an integer literal, and can be surrounded by whitespace. Again, upon lexing a single bracketed index expression, we want to pop back to dqt mode directly.

<dqs> @name / \[   { _string Lex.SC_Name (pushMode dqb) }

<dqb> \[       { _symbol Lex.SY_OpenBrack (stashDelimiter Brack cmd (popMode >> popMode)) }

-- And lastly, any variable name not followed by '->' or '[' represents itself.

<dqs> @name         { _string Lex.SC_Name popMode }



-- Complex Expressions
----------------------

-- Complex expressions are a little simpler than simple expressions; go figure. There are two ways to enter complex mode -- '${' and '{$name' -- and one way to exit.

<dqt> \$\{                  { _symbol Lex.SY_DollarOpenBrace (stashDelimiter Brace cmd ok) }
<dqt> \{ / \$$nameNondigit  { _symbol Lex.SY_OpenBrace       (stashDelimiter Brace cmd ok) }



-- Text in Double Quoted Strings
--------------------------------

-- Last but not least -- any other character in a double quoted string is interpreted literally. We have to match `\n` explicitly because Alex does not match it against `.`.

<dqt>  [.\n]              { _anychar Lex.CC_DoubleQuotedString }



-- Heredocs --
--------------

-- Heredocs are one of the more delicate parts of PHP's syntax. Functionally they are very similar to double quoted strings, except that the delimiter pair is not known in advance.

<cmd>  \<\<\< / ["$nameNondigit]      { _symbol       Lex.SY_TripleLess  (pushMode hdo)  }

<hdo>  "     / $nameNondigit          { _symbol       Lex.SY_DoubleQuote ok              }
<hdo>  @name / "                      { _queueDocName                    Here ok         }
<hdo>  "     / [\r\n] # $nameNondigit { _symbol       Lex.SY_DoubleQuote ok        }
<hdo>  @name / [\r\n] # ["]           { _queueDocName                    Here ok }

<hdo>  [\n]     / @name\;?[\r\n]  { _newline      Lex.NL_LF          (docStart >> pushMode hde) }
<hdo>  [\r]     / @name\;?[\r\n]  { _newline      Lex.NL_CR          (docStart >> pushMode hde) }
<hdo>  [\r][\n] / @name\;?[\r\n]  { _newline      Lex.NL_CRLF        (docStart >> pushMode hde) }

<hdo>  [\n]       { _newline      Lex.NL_LF          (docStart) }
<hdo>  [\r]       { _newline      Lex.NL_CR          (docStart) }
<hdo>  [\r][\n]   { _newline      Lex.NL_CRLF        (docStart) }

-- If we encounter a newline followed by a name character, we enter a provisional end lexing mode to see if we have an end token.

<hdc>  [\n]     / @name\;?[\r\n]  { _newline      Lex.NL_LF          (pushMode hde) }
<hdc>  [\r]     / @name\;?[\r\n]  { _newline      Lex.NL_CR          (pushMode hde) }
<hdc>  [\r][\n] / @name\;?[\r\n]  { _newline      Lex.NL_CRLF        (pushMode hde) }

<hdc>  [\n]                       { _anychar      Lex.CC_Heredoc       }
<hdc>  [\r]                       { _anychar      Lex.CC_Heredoc       }

-- The special _heredocEnd action compares a lexed name followed by a semicolon and newline to the current document name. If it matches, then this heredoc ends; otherwise we continue lexing heredoc tokens.

<hde>  @name / \;?[\r\n]          { _heredocEnd }

-- Escape Sequences
-------------------

<hdc>  \\\\     { _escaped Lex.ES_DoubleQuotedString_Backslash      }
<hdc>  \\\$     { _escaped Lex.ES_DoubleQuotedString_Dollar         }
<hdc>  \\e      { _escaped Lex.ES_DoubleQuotedString_Escape         }
<hdc>  \\f      { _escaped Lex.ES_DoubleQuotedString_FormFeed       }
<hdc>  \\n      { _escaped Lex.ES_DoubleQuotedString_LineFeed       }
<hdc>  \\r      { _escaped Lex.ES_DoubleQuotedString_CarriageReturn }
<hdc>  \\t      { _escaped Lex.ES_DoubleQuotedString_HorizontalTab  }
<hdc>  \\v      { _escaped Lex.ES_DoubleQuotedString_VerticalTab    }

<hdc>  \\[$oct]{1,3}   { _string Lex.SC_OctalEscape   ok }
<hdc>  \\[xX][$hex]+   { _string Lex.SC_HexEscape     ok }
<hdc>  \\u\{[$hex]+\}  { _string Lex.SC_UnicodeEscape ok }



-- Expressions
--------------

<hdc>  \$ / $nameNondigit { _symbol Lex.SY_Dollar (pushMode hds) }

<hds>  @name / \->    { _string Lex.SC_Name         (pushMode hda)       }

<hda>  \->            { _symbol Lex.SY_MinusGreater ok                   }
<hda>  @name          { _string Lex.SC_Name         (popMode >> popMode) }

<hds>  @name / \[     { _string Lex.SC_Name (pushMode hdb) }

<hdb>  \[             { _symbol Lex.SY_OpenBrack (stashDelimiter Brack cmd (popMode >> popMode)) }

<hds>  @name          { _string Lex.SC_Name popMode }

<hdc>  \$\{                  { _symbol Lex.SY_DollarOpenBrace (stashDelimiter Brace cmd ok) }
<hdc>  \{ / \$$nameNondigit  { _symbol Lex.SY_OpenBrace       (stashDelimiter Brace cmd ok) }

<hdc>  [.\n]                 { _anychar Lex.CC_Heredoc }





-- Command Mode --
------------------

<cmd>  \?\>                { _phptag Lex.TG_End popMode }

-- Whitespace
-------------

<cmd>  [\ ]                { _whitespace Lex.WS_Space }
<cmd>  [\t]                { _whitespace Lex.WS_HorizontalTab }

<cmd>  [\n]                { _newline Lex.NL_LF   ok }
<cmd>  [\r]                { _newline Lex.NL_CR   ok }
<cmd>  [\r][\n]            { _newline Lex.NL_CRLF ok }



<cmd>  B / ['"]            { _prefix Lex.PX_String_LargeB ok }
<cmd>  b / ['"]            { _prefix Lex.PX_String_SmallB ok }
<cmd>  B / \<\<\<          { _prefix Lex.PX_String_LargeB ok }
<cmd>  b / \<\<\<          { _prefix Lex.PX_String_SmallB ok }



-- Variables
------------

-- Keywords can be used as variable names; we use a separate lexical mode to allow this.

<cmd>  \$        { _symbol Lex.SY_Dollar (pushMode var) }

<var>  \$        { _symbol Lex.SY_Dollar ok }
<var>  @name     { _string Lex.SC_Name popMode }
<var>  \{        { _symbol Lex.SY_OpenBrace (popMode >> pushDelimiter Brace) }



-- Keywords --
--------------

<cmd>  $A$B$S$T$R$A$C$T          { _keyword Lex.KW_Abstract    ok }
<cmd>  $A$N$D                    { _keyword Lex.KW_And         ok }
<cmd>  $A$R$R$A$Y                { _keyword Lex.KW_Array       ok }
<cmd>  $A$S                      { _keyword Lex.KW_As          ok }
<cmd>  $B$I$N$A$R$Y              { _keyword Lex.KW_Binary      ok }
<cmd>  $B$O$O$L                  { _keyword Lex.KW_Bool        ok }
<cmd>  $B$O$O$L$E$A$N            { _keyword Lex.KW_Boolean     ok }
<cmd>  $B$R$E$A$K                { _keyword Lex.KW_Break       ok }
<cmd>  $C$A$L$L$A$B$L$E          { _keyword Lex.KW_Callable    ok }
<cmd>  $C$A$S$E                  { _keyword Lex.KW_Case        ok }
<cmd>  $C$A$T$C$H                { _keyword Lex.KW_Catch       ok }
<cmd>  $C$L$A$S$S                { _keyword Lex.KW_Class       ok }
<cmd>  $C$L$O$N$E                { _keyword Lex.KW_Clone       ok }
<cmd>  $C$O$N$S$T                { _keyword Lex.KW_Const       ok }
<cmd>  __$C$O$N$S$T$R$U$C$T      { _keyword Lex.KW_Construct   ok }
<cmd>  $C$O$N$T$I$N$U$E          { _keyword Lex.KW_Continue    ok }
<cmd>  $D$E$C$L$A$R$E            { _keyword Lex.KW_Declare     ok }
<cmd>  $D$E$F$A$U$L$T            { _keyword Lex.KW_Default     ok }
<cmd>  __$D$E$S$T$R$U$C$T        { _keyword Lex.KW_Destruct    ok }
<cmd>  $D$I$E                    { _keyword Lex.KW_Die         ok }
<cmd>  $D$O                      { _keyword Lex.KW_Do          ok }
<cmd>  $D$O$U$B$L$E              { _keyword Lex.KW_Double      ok }
<cmd>  $E$C$H$O                  { _keyword Lex.KW_Echo        ok }
<cmd>  $E$L$S$E                  { _keyword Lex.KW_Else        ok }
<cmd>  $E$L$S$E$I$F              { _keyword Lex.KW_Elseif      ok }
<cmd>  $E$M$P$T$Y                { _keyword Lex.KW_Empty       ok }
<cmd>  $E$N$C$O$D$I$N$G          { _keyword Lex.KW_Encoding    ok }
<cmd>  $E$N$D$D$E$C$L$A$R$E      { _keyword Lex.KW_Enddeclare  ok }
<cmd>  $E$N$D$F$O$R$E$A$C$H      { _keyword Lex.KW_Endforeach  ok }
<cmd>  $E$N$D$F$O$R              { _keyword Lex.KW_Endfor      ok }
<cmd>  $E$N$D$I$F                { _keyword Lex.KW_Endif       ok }
<cmd>  $E$N$D$S$W$I$T$C$H        { _keyword Lex.KW_Endswitch   ok }
<cmd>  $E$N$D$W$H$I$L$E          { _keyword Lex.KW_Endwhile    ok }
<cmd>  $E$V$A$L                  { _keyword Lex.KW_Eval        ok }
<cmd>  $E$X$I$T                  { _keyword Lex.KW_Exit        ok }
<cmd>  $E$X$T$E$N$D$S            { _keyword Lex.KW_Extends     ok }
<cmd>  $F$I$N$A$L                { _keyword Lex.KW_Final       ok }
<cmd>  $F$I$N$A$L$L$Y            { _keyword Lex.KW_Finally     ok }
<cmd>  $F$L$O$A$T                { _keyword Lex.KW_Float       ok }
<cmd>  $F$O$R$E$A$C$H            { _keyword Lex.KW_Foreach     ok }
<cmd>  $F$O$R                    { _keyword Lex.KW_For         ok }
<cmd>  $F$U$N$C$T$I$O$N          { _keyword Lex.KW_Function    ok }
<cmd>  $G$L$O$B$A$L              { _keyword Lex.KW_Global      ok }
<cmd>  $G$O$T$O                  { _keyword Lex.KW_Goto        ok }
<cmd>  $I$F                      { _keyword Lex.KW_If          ok }
<cmd>  $I$M$P$L$E$M$E$N$T$S      { _keyword Lex.KW_Implements  ok }
<cmd>  $I$N$C$L$U$D$E            { _keyword Lex.KW_Include     ok }
<cmd>  $I$N$C$L$U$D$E[_]$O$N$C$E { _keyword Lex.KW_IncludeOnce ok }
<cmd>  $I$N$S$T$A$N$C$E$O$F      { _keyword Lex.KW_Instanceof  ok }
<cmd>  $I$N$S$T$E$A$D$O$F        { _keyword Lex.KW_Insteadof   ok }
<cmd>  $I$N$T                    { _keyword Lex.KW_Int         ok }
<cmd>  $I$N$T$E$G$E$R            { _keyword Lex.KW_Integer     ok }
<cmd>  $I$N$T$E$R$F$A$C$E        { _keyword Lex.KW_Interface   ok }
<cmd>  $I$S$S$E$T                { _keyword Lex.KW_Isset       ok }
<cmd>  $I$T$E$R$A$B$L$E          { _keyword Lex.KW_Iterable    ok }
<cmd>  $L$I$S$T                  { _keyword Lex.KW_List        ok }
<cmd>  $N$A$M$E$S$P$A$C$E        { _keyword Lex.KW_Namespace   ok }
<cmd>  $N$E$W                    { _keyword Lex.KW_New         ok }
<cmd>  $O$B$J$E$C$T              { _keyword Lex.KW_Object      ok }
<cmd>  $O$R                      { _keyword Lex.KW_Or          ok }
<cmd>  $P$A$R$E$N$T              { _keyword Lex.KW_Parent      ok }
<cmd>  $P$R$I$N$T                { _keyword Lex.KW_Print       ok }
<cmd>  $P$R$I$V$A$T$E            { _keyword Lex.KW_Private     ok }
<cmd>  $P$R$O$T$E$C$T$E$D        { _keyword Lex.KW_Protected   ok }
<cmd>  $P$U$B$L$I$C              { _keyword Lex.KW_Public      ok }
<cmd>  $R$E$A$L                  { _keyword Lex.KW_Real        ok }
<cmd>  $R$E$Q$U$I$R$E            { _keyword Lex.KW_Require     ok }
<cmd>  $R$E$Q$U$I$R$E[_]$O$N$C$E { _keyword Lex.KW_RequireOnce ok }
<cmd>  $R$E$T$U$R$N              { _keyword Lex.KW_Return      ok }
<cmd>  $S$E$L$F                  { _keyword Lex.KW_Self        ok }
<cmd>  $S$T$A$T$I$C              { _keyword Lex.KW_Static      ok }
<cmd>  $S$T$R$I$C$T[_]$T$Y$P$E$S { _keyword Lex.KW_StrictTypes ok }
<cmd>  $S$T$R$I$N$G              { _keyword Lex.KW_String      ok }
<cmd>  $S$W$I$T$C$H              { _keyword Lex.KW_Switch      ok }
<cmd>  $T$H$R$O$W                { _keyword Lex.KW_Throw       ok }
<cmd>  $T$I$C$K$S                { _keyword Lex.KW_Ticks       ok }
<cmd>  $T$R$A$I$T                { _keyword Lex.KW_Trait       ok }
<cmd>  $T$R$Y                    { _keyword Lex.KW_Try         ok }
<cmd>  $U$N$S$E$T                { _keyword Lex.KW_Unset       ok }
<cmd>  $U$S$E                    { _keyword Lex.KW_Use         ok }
<cmd>  $V$A$R                    { _keyword Lex.KW_Var         ok }
<cmd>  $V$O$I$D                  { _keyword Lex.KW_Void        ok }
<cmd>  $W$H$I$L$E                { _keyword Lex.KW_While       ok }
<cmd>  $Y$I$E$L$D                { _keyword Lex.KW_Yield       ok }
<cmd>  $Y$I$E$L$D[\ ]$F$R$O$M    { _keyword Lex.KW_YieldFrom   ok }
<cmd>  $X$O$R                    { _keyword Lex.KW_Xor         ok }



-- Symbols --
-------------

-- Note that curly braces and square brackets are used as semantic delimiters.

<cmd>  \{        { _symbol Lex.SY_OpenBrace   (pushDelimiter Brace) }
<cmd>  \}        { _symbol Lex.SY_ClosedBrace (popDelimiter Brace)  }

<cmd>  \[        { _symbol Lex.SY_OpenBrack   (pushDelimiter Brack) }
<cmd>  \]        { _symbol Lex.SY_ClosedBrack (popDelimiter Brack)  }

<cmd>  &&        { _symbol Lex.SY_AmpAmp             ok }
<cmd>  &=        { _symbol Lex.SY_AmpEqual           ok }
<cmd>  &         { _symbol Lex.SY_Amp                ok }
<cmd>  \*        { _symbol Lex.SY_Ast                ok }
<cmd>  \*\*      { _symbol Lex.SY_AstAst             ok }
<cmd>  \*\*=     { _symbol Lex.SY_AstAstEqual        ok }
<cmd>  \*=       { _symbol Lex.SY_AstEqual           ok }
<cmd>  @         { _symbol Lex.SY_At                 ok }
<cmd>  \\        { _symbol Lex.SY_Backslash          ok }
<cmd>  `         { _symbol Lex.SY_Backtick           ok }
<cmd>  !         { _symbol Lex.SY_Bang               ok }
<cmd>  !=        { _symbol Lex.SY_BangEqual          ok }
<cmd>  !==       { _symbol Lex.SY_BangEqualEqual     ok }
<cmd>  \^        { _symbol Lex.SY_Caret              ok }
<cmd>  \^=       { _symbol Lex.SY_CaretEqual         ok }
<cmd>  \)        { _symbol Lex.SY_ClosedParen        ok }
<cmd>  :         { _symbol Lex.SY_Colon              ok }
<cmd>  \,        { _symbol Lex.SY_Comma              ok }
<cmd>  \.        { _symbol Lex.SY_Dot                ok }
<cmd>  \.=       { _symbol Lex.SY_DotEqual           ok }
<cmd>  ::        { _symbol Lex.SY_DoubleColon        ok }
<cmd>  ==        { _symbol Lex.SY_DoubleEqual        ok }
<cmd>  >>        { _symbol Lex.SY_DoubleGreater      ok }
<cmd>  >>=       { _symbol Lex.SY_DoubleGreaterEqual ok }
<cmd>  \<\<      { _symbol Lex.SY_DoubleLess         ok }
<cmd>  \<\<=     { _symbol Lex.SY_DoubleLessEqual    ok }
<cmd>  \-\-      { _symbol Lex.SY_DoubleMinus        ok }
<cmd>  \|\|      { _symbol Lex.SY_DoublePipe         ok }
<cmd>  \+\+      { _symbol Lex.SY_DoublePlus         ok }
<cmd>  \?\?      { _symbol Lex.SY_DoubleQuestion     ok }
<cmd>  "         { _symbol Lex.SY_DoubleQuote        ok }
<cmd>  \.\.\.    { _symbol Lex.SY_Ellipsis           ok }
<cmd>  =         { _symbol Lex.SY_Equal              ok }
<cmd>  =>        { _symbol Lex.SY_EqualGreater       ok }
<cmd>  >         { _symbol Lex.SY_Greater            ok }
<cmd>  >=        { _symbol Lex.SY_GreaterEqual       ok }
<cmd>  \<        { _symbol Lex.SY_Less               ok }
<cmd>  \<=       { _symbol Lex.SY_LessEqual          ok }
<cmd>  \<>       { _symbol Lex.SY_LessGreater        ok }
<cmd>  \-        { _symbol Lex.SY_Minus              ok }
<cmd>  \-=       { _symbol Lex.SY_MinusEqual         ok }
<cmd>  \->       { _symbol Lex.SY_MinusGreater       ok }
<cmd>  \(        { _symbol Lex.SY_OpenParen          ok }
<cmd>  \%        { _symbol Lex.SY_Percent            ok }
<cmd>  \%=       { _symbol Lex.SY_PercentEqual       ok }
<cmd>  \|        { _symbol Lex.SY_Pipe               ok }
<cmd>  \|=       { _symbol Lex.SY_PipeEqual          ok }
<cmd>  \+        { _symbol Lex.SY_Plus               ok }
<cmd>  \+=       { _symbol Lex.SY_PlusEqual          ok }
<cmd>  \?        { _symbol Lex.SY_Question           ok }
<cmd>  \;        { _symbol Lex.SY_Semicolon          ok }
<cmd>  '         { _symbol Lex.SY_SingleQuote        ok }
<cmd>  \/        { _symbol Lex.SY_Slash              ok }
<cmd>  \/=       { _symbol Lex.SY_SlashEqual         ok }
<cmd>  \<=>      { _symbol Lex.SY_Spaceship          ok }
<cmd>  \~        { _symbol Lex.SY_Tilde              ok }
<cmd>  ===       { _symbol Lex.SY_TripleEqual        ok }



-- Identifiers
--------------

-- Finally we lex arbitrary names. This recognizer must come after the keyword recognizers; otherwise all keywords are lexed as identifiers.

<cmd>  @name     { _string Lex.SC_Name ok }


       [.\n]     { _lexError }





-- ============= --
-- Lexical State --
-- ============= --

-- Recall that we need to track three kinds of state: the lexical mode, the lexical context, and the delimited context. We can now define this state and the functions for manipulating it.

{

-- Alex works by constructing an enormous monadic state machine behind the scenes, and provides a mechanism for threading our own custom state through the lexing process. All we have to do is call our custom state type `AlexUserState`, and interact with it within a built-in `Alex` monad.

data AlexUserState = AlexUserState
  { startcodeStack :: [Int]
  , currentDocName :: [(Doc, String)]
  , delimiterStack :: [(Delimiter, Maybe Stash)]
  , nextDocName    :: Maybe (Doc, String)
  , debugTrace     :: [(LexMode, Trace)]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { startcodeStack = []
  , currentDocName = []
  , delimiterStack = []
  , nextDocName    = Nothing
  , debugTrace     = []
  }

-- Alex already keeps track of the current lexical mode, and `startcodeStack` is the current lexical context. `delimiterStack` is the delimited context stack.

type LexMode = Int

-- The `Delimiter` type represents the kinds of semantic delimiters we need. Remember these are pairs of input tokens that delimit an isolated inner lexical context.

data Delimiter
  = Brace
  | Brack
  | Doc Doc String
  deriving (Eq, Show)

data Doc
  = Here
  | Now
  deriving (Eq, Show)

-- The `Stash` type represents a stored lexical mode and context, as well as a monadic action to be performed _after_ the context is restored. The stored action makes it possible to queue up some mutation to perform on the restored context in advance, like popping the mode.

data Stash
  = Stash LexMode [LexMode] (Alex ())

-- Very often the stored action will be trivial; we call this `ok`.

ok :: Alex ()
ok = return ()

-- `gets` and `muts` are the usual utility functions for retrieving and altering the threaded state, and `lexError` is a utility for reporting lexical errors.

gets :: (AlexState -> a) -> Alex a
gets f = Alex $ \ast ->
  Right (ast, f ast)

muts :: (AlexState -> AlexState) -> Alex ()
muts f = Alex $ \ast ->
  Right (f ast, ())

lexError :: String -> Alex a
lexError msg = do
  showModes >>= \modes -> Alex $ \_ ->
    Left $ msg ++ "\n" ++ modes



-- State Manipulation --
------------------------

-- First we define utilities for manipulating the lexical context: we need to push modes onto this stack, pop the top mode off (defaulting to `0` if the stack is empty), and flush the stack (for the case of ending a single line comment with a closing PHP tag).

pushMode :: LexMode -> Alex ()
pushMode k = do
  trace $ T_PushMode k
  ust  <- gets alex_ust
  mode <- gets alex_scd
  let stack = startcodeStack ust
  muts $ \ast -> ast
    { alex_scd = k
    , alex_ust = ust
      { startcodeStack = mode : stack
      }
    }

popMode :: Alex ()
popMode = do
  trace $ T_PopMode
  ust <- gets alex_ust
  let (k,stack) = case startcodeStack ust of
        []   -> (0,[])
        a:as -> (a,as)
  muts $ \ast -> ast
    { alex_scd = k
    , alex_ust = ust
      { startcodeStack = stack
      }
    }

flushMode :: Alex ()
flushMode = do
  ust <- gets alex_ust
  muts $ \ast -> ast
    { alex_scd = 0
    , alex_ust = ust
      { startcodeStack = []
      }
    }



-- Next we define utilities for manipulating the delimited context. Here we need two different kinds of pushes, because we may or may not want to stash the lexical context when pushing a semantic delimiter.

pushDelimiter :: Delimiter -> Alex ()
pushDelimiter d = do
  ust <- gets alex_ust
  let dstack = delimiterStack ust
  muts $ \ast -> ast
    { alex_ust = ust
      { delimiterStack = (d, Nothing) : dstack
      }
    }

stashDelimiter :: Delimiter -> LexMode -> Alex () -> Alex ()
stashDelimiter d scd act = do
  ust <- gets alex_ust
  sco <- gets alex_scd
  let
    dstack = delimiterStack ust
    sstack = startcodeStack ust
  muts $ \ast -> ast
    { alex_scd = scd
    , alex_ust = ust
      { startcodeStack = []
      , delimiterStack = (d, Just (Stash sco sstack act)) : dstack
      }
    }

popDelimiter :: Delimiter -> Alex ()
popDelimiter d = do
  ust <- gets alex_ust
  let dstack = delimiterStack ust
  case dstack of
    [] -> lexError "Mismatched delimiter"
    (d',x):ds -> if d /= d'
      then lexError "Unexpexted delimiter"
      else case x of
        Nothing -> muts $ \ast -> ast
          { alex_ust = ust
            { delimiterStack = ds
            }
          }
        Just (Stash a as act) -> do
          muts $ \ast -> ast
            { alex_scd = a
            , alex_ust = ust
              { startcodeStack = as
              , delimiterStack = ds
              }
            }
          act



peekDocName :: Alex (Maybe (Doc, String))
peekDocName = do
  ust <- gets alex_ust
  return $ case currentDocName ust of
    []  -> Nothing
    x:_ -> Just x

pushDocName :: Doc -> String -> Alex ()
pushDocName doc name = do
  ust <- gets alex_ust
  let nstack = currentDocName ust
  muts $ \ast -> ast
    { alex_ust = ust
      { currentDocName = (doc, name) : nstack
      }
    }
  stashDelimiter (Doc doc name) (case doc of Here -> hdc; Now -> 0) ok

popDocName :: Doc -> String -> Alex ()
popDocName doc name = do
  popDelimiter (Doc doc name)
  ust <- gets alex_ust
  let nstack = currentDocName ust
  case nstack of
    [] -> lexError "mismatched doc name pop"
    _:ns -> muts $ \ast -> ast
      { alex_ust = ust
        { currentDocName = ns
        }
      }

setNextDocName :: Doc -> String -> Alex ()
setNextDocName doc str = do
  ust <- gets alex_ust
  muts $ \ast -> ast
    { alex_ust = ust
      { nextDocName = Just (doc, str)
      }
    }

getNextDocName :: Alex (Maybe (Doc, String))
getNextDocName = do
  ust <- gets alex_ust
  muts $ \ast -> ast
    { alex_ust = ust
      { nextDocName = Nothing
      }
    }
  return $ nextDocName ust





-- ================ --
-- The Parser Monad --
-- ================ --

-- Later stages of our compiler will interact with the lexer via the `Parser` monad. The main interface for this is `lexer`, whose signature is dictated by the parser generator we're using.

lexer :: (Token -> Parser a) -> Parser a
lexer = ((Parser $ fmap Right alexMonadScan) >>=)

-- The parser will operate on a uniform `Token` type; defined here.

data Token
  = TokenEndLine    Lex.TokenNewLine     Loc
  | TokenWhiteSpace Lex.TokenWhiteSpace  Loc
  | TokenKeyword    Lex.TokenKeyword     (Loc, String)
  | TokenSymbol     Lex.TokenSymbol      Loc
  | TokenAnyChar    Lex.TokenCharClass   (Loc, Char)
  | TokenPhpTag     Lex.TokenPhpTag      Loc
  | TokenPrefix     Lex.TokenPrefix      Loc
  | TokenEscaped    Lex.TokenEscaped     Loc
  | TokenString     Lex.TokenStringClass (Loc, String)

  | EOF
  deriving (Eq, Show)

-- And the `Parser` monad is a basic stack under `Alex` with an additional error value; the monad instance for this type is straightforward.

newtype Parser a = Parser
  { unParser :: Alex (Either Token a)
  }

instance Functor Parser where
  fmap f = Parser . fmap (fmap f) . unParser

instance Applicative Parser where
  pure = Parser . pure . Right

  f <*> x = Parser $ do
    f' <- unParser f
    x' <- unParser x
    return $ f' <*> x'

instance Monad Parser where
  return = Parser . return . Right

  x >>= f = Parser $ do
    a <- unParser x
    case a of
      Left err -> return $ Left err
      Right ok -> unParser $ f ok


alexEOF :: Alex Token
alexEOF = return EOF



-- Action Helpers
-----------------

-- Finally, we define utility functions for defining the token constructors.

_keyword
  :: Lex.TokenKeyword
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_keyword kw action (p,_,_,s) i = do
  let token = TokenKeyword kw (toLoc p, take i s)
  trace $ T_Recognize token
  action
  return token

_newline
  :: Lex.TokenNewLine
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_newline el action (p,_,_,_) _ = do
  let token = TokenEndLine el (toLoc p)
  trace $ T_Recognize token
  action
  return token

_symbol
  :: Lex.TokenSymbol
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_symbol sy action (p,_,_,_) _ = do
  let token = TokenSymbol sy (toLoc p)
  trace $ T_Recognize token
  action
  return token

_phptag
  :: Lex.TokenPhpTag
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_phptag tg action (p,_,_,_) _ = do
  let token = TokenPhpTag tg (toLoc p)
  trace $ T_Recognize token
  action
  return token

_whitespace
  :: Lex.TokenWhiteSpace
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_whitespace ws (p,_,_,_) _ = do
  let token = TokenWhiteSpace ws (toLoc p)
  trace $ T_Recognize token
  return token

_anychar
  :: Lex.TokenCharClass
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_anychar cc (p,_,_,s) i =
  case i of
    1 -> do
      let token = TokenAnyChar cc (toLoc p, head s)
      trace $ T_Recognize token
      return token
    _ -> Alex $ \s -> Left "Lexical error: expected to lex a single character"

_prefix
  :: Lex.TokenPrefix
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_prefix px action (p,_,_,s) i = do
  let token = TokenPrefix px (toLoc p)
  trace $ T_Recognize token
  action
  return token

_string
  :: Lex.TokenStringClass
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_string sc action (p,_,_,s) i = do
  let token = TokenString sc (toLoc p, take i s)
  trace $ T_Recognize token
  action
  return token

_escaped
  :: Lex.TokenEscaped
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_escaped es (p,_,_,s) i = do
  let token = TokenEscaped es (toLoc p)
  trace $ T_Recognize token
  return token

_lexError
  :: (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex a
_lexError (p,_,_,s) i = do
  let
    msg = concat
      [ "unexpected \"", take i s, "\" at ", prettyDebug (toLoc p)
      ]
  lexError msg

_queueDocName
  :: Doc
  -> Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_queueDocName doc action (p,_,_,s) i = do
  let
    name = take i s
    token = TokenString Lex.SC_DocStart (toLoc p, name)
  trace $ T_QueueName (toLoc p) name
  setNextDocName doc name
  action
  return token

docStart :: Alex ()
docStart = do
  x <- getNextDocName
  case x of
    Nothing -> do
      lexError "No doc name queued!"
    Just (d,s) -> do
      trace $ T_DocStart d s
      pushDocName d s

_heredocStart
  :: Alex ()
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_heredocStart action (p,_,_,s) i = do
  let
    name = take i s
    token = TokenString Lex.SC_DocStart (toLoc p, name)
  trace $ T_Recognize token
  pushDocName Here name
  action
  return token

_heredocEnd
  :: (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_heredocEnd (p,_,_,s) i = do
  name <- peekDocName
  case name of
    Nothing -> lexError "unexpected end of heredoc"
    Just (Now,_) -> lexError "expected to end heredoc"
    Just (Here,ns) -> do
      let this = take i s
      if this == ns
        then do
          trace $ T_DocEnd this
          popDocName Here this
          popMode -- return to cmd mode
          return $ TokenString Lex.SC_DocEnd (toLoc p, this)
        else do
          popMode -- return to hdc mode
          return $ TokenString Lex.SC_DocString (toLoc p, this)


showModes :: Alex String
showModes = do
  st <- gets id
  return $ showUserState st

toLoc :: AlexPosn -> Loc
toLoc (AlexPn a b c) = Loc
  { _line = b
  , _col  = c
  , _char = a
  }

showUserState :: AlexState -> String
showUserState ast =
  let
    lexical_mode = concat
      [ "lexical_mode: "
      , showStartcode (alex_scd ast), "\n"
      ]
    lexical_context = concat
      [ "lexical_context: "
      , L.intercalate ", " $
          map showStartcode (startcodeStack $ alex_ust ast)
      , "\n"
      ]
    delimited_context = concat
      [ "delimited_context:\n  "
      , L.intercalate "\n  " $
          map showDelimCtx (delimiterStack $ alex_ust ast)
      , "\n"
      ]
    debug_trace = concat
      [ "trace:\n  "
      , L.intercalate "\n  " $
          map (\(scd,x) -> showStartcode scd ++ " " ++ prettyDebug x)
          (debugTrace $ alex_ust ast)
      , "\n"
      ]
  in
    concat
      [ lexical_mode
      , lexical_context
      , delimited_context
      , debug_trace
      ]

instance PrettyDebug AlexState where
  prettyDebug = showUserState

showDelimCtx :: (Delimiter, Maybe Stash) -> String
showDelimCtx (d, x) = case x of
  Nothing -> show d
  Just (Stash a as _) -> show d ++ show a ++ show as

showStartcode :: Int -> String
showStartcode k =
  let
    codes =
      [ (cmd, "cmd")
      , (hdc, "hdc")
      , (hdo, "hdo")
      , (hde, "hde")
      , (hda, "hda")
      , (hdb, "hdb")
      , (hds, "hds")
      , (dqt, "dqt")
      , (dqs, "dqs")
      , (dqa, "dqa")
      , (dqb, "dqb")
      , (slc, "slc")
      , (mlc, "mlc")
      , (pow, "pow")
      , (bin, "bin")
      , (hex, "hex")
      , (oct, "oct")
      , (sqt, "sqt")
      , (var, "var")
      , (0,   "0  ")
      ]
  in
    case L.lookup k codes of
      Nothing -> show k
      Just sc -> sc

data Trace
  = T_Recognize Token
  | T_PushMode LexMode
  | T_PopMode
  | T_QueueName Loc String
  | T_DocStart Doc String
  | T_DocEnd String
  | T_Reduce String
  deriving (Eq, Show)

trace :: Trace -> Alex ()
trace x = if _DEBUG_TRACE
  then do
    ust <- gets alex_ust
    scd <- gets alex_scd
    let tr = debugTrace ust
    muts $ \ast -> ast
      { alex_ust = ust
        { debugTrace = (scd, x) : tr
        }
      }
  else return ()

_DEBUG_TRACE :: Bool
_DEBUG_TRACE = True

class PrettyDebug t where
  prettyDebug :: t -> String

instance PrettyDebug Delimiter where
  prettyDebug = show

instance PrettyDebug Trace where
  prettyDebug x = case x of
    T_Recognize tok     -> " recognize   " ++ prettyDebug tok
    T_PushMode mode     -> " push_mode   " ++ showStartcode mode
    T_PopMode           -> " pop_mode"
    T_QueueName loc str -> " queue_name  '" ++ str ++ "' at " ++ prettyDebug loc
    T_DocStart doc name -> " start_doc   " ++ show doc ++ " '" ++ name ++ "'"
    T_DocEnd name       -> " end_doc     '" ++ name ++ "'"
    T_Reduce msg        -> " reduce      " ++ msg

instance PrettyDebug Loc where
  prettyDebug x = concat
    [ show (_line x), ":", show (_col x), "(", show (_char x), ")"
    ]

instance PrettyDebug Token where
  prettyDebug x = case x of
    TokenEndLine    z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenWhiteSpace z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenKeyword    z (l,s) -> concat [ show z, " '", s, "' at ", prettyDebug l ]
    TokenSymbol     z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenAnyChar    z (l,c) -> concat [ show z, " '", prettyDebug c, "' at ", prettyDebug l ]
    TokenPhpTag     z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenPrefix     z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenEscaped    z l     -> concat [ show z, " at ", prettyDebug l ]
    TokenString     z (l,s) -> concat [ show z, " '", s, "' at ", prettyDebug l ]

    EOF -> "EOF"

instance PrettyDebug Char where
  prettyDebug c = case c of
    '\r' -> "\\r"
    '\n' -> "\\n"
    '\t' -> "\\t"
    '\v' -> "\\v"
    c    -> [c]

debugAlex
  :: String -> Alex a -> Either String (AlexState, a)
debugAlex input__ (Alex f) = f $ AlexState
  { alex_bytes = []
  , alex_pos   = alexStartPos
  , alex_inp   = input__
  , alex_chr   = '\n'
  , alex_ust   = alexInitUserState
  , alex_scd   = 0
  }

}
