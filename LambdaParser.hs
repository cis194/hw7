module LambdaParser where

import LambdaCalc
import Parser
import Control.Applicative (Alternative(..))
import Control.Monad (unless)


{- 7.0 Parsing the the Lambda Calculus -}

{-
In the last assignment, you wrote an evaluator for the lambda calculus. In
addition, we added pretty printing for lambda calculus expressions, so certain
values of type Expr (ex: lambda encodings for "true" and "false")
have nicer displays as strings. We did this by defining a show instance for lambda
expressions. Now, let's do the opposite by defining a parser which takes strings
and converts them to values of type Expr. Once we have parser, we can feed
it arbitrary strings and have it return a lambda calculus expression or fail with
an error. Haskell's emphasis on function composition lends itself nicely to writing
parsers due to their plug-and-play recursive structure.

We will be using the Parser module as our parsing library, which is heavily
based off of the parsing library we developed in lec9. This module has a
similar API to the ubiquitous Parsec package but is simpler (and is
subsequently less performant). Before you start working on this file,
I entreat you to read through the Parser.hs file and make sure you have a
working understanding of all of the definitions there. Being familiar with
the Parser API will make this assignment much, much easier. While you should
feel free to modify and extend the Parser module as you see fit, all the
primitives and combinators you should need are already provided (along with
the Monad instance for Parser). Try to take advantage of what we gave you.
-}

{-
The goal of this assignment is to parse well-formed Strings into the
lambda expressions that they represent. In programming language theory
this might be referred to as building an "abstract syntax tree" (AST)
by parsing the code for a program.

We will then pass the AST into the eval function from the previous homework
to simplify to get the result of evaluating the user's program.
-}

parseAndEval :: String -> IO ()
parseAndEval = eval . parse

{-
First, we define a function which accepts an arbitrary String and returns
a value of type Expr if it is well formed (and fails otherwise). In this case,
failing means crashing with an error message. In larger systems, ususally
there is a special error type that carries error information during parsing
so failure is more graceful.
-}

parse :: String -> Expr
parse input =
  case runParser parseExpr input of
    Just (e, []) -> e
    _ -> error "Invalid input syntax!"

{-
Now, we need to define a parser for values of type Expr.

Writing a parser for values of type Expr means defining parsers for each of the
three constructors of the Expr type, namely Var, Apply, and Lambda.

Once we define the parsers for each of the constructors in Expr, we can
glue all three of our individual constructor parsers together
to parse a value of type Expr using the (<|>) combinator. Checkout Parser.hs
for the Parser instance of Alternative to refresh your memory
of how this combinator is defined for the Parser type. Because of
the way we will describe our grammar, the order of the constructor
parsers in the (<|>) combinator chain below does not matter.
As you progress through the assignment, you may want to think about why.

Additionally, you should try to be lenient about extra spaces between the
different parts of the string. Therefore "\x.   x" and "\x.x" should
both parse to `Lambda "x" (Var "x")`.
-}

parseExpr :: Parser Expr
parseExpr = parseLambda <|> parseApplication <|> parseVar

{-
The parser specification for identifiers is any string of one or more
characters starting with an alphabetic character with the remainder
consisting of alphanumeric characters. We are going to use this parser
as a helper for the parseVar and parseLambda parsers.
-}

parseIdent :: Parser Ident
parseIdent = undefined

{-
Var Rule: <Ident> --> Var <Ident>

Examples:
"x" --> Var "x"
"myC00lV4r14ble" --> Var "myC00lV4r14ble"
"8iStartWithADigit" --> ERROR
-}

parseVar :: Parser Expr
parseVar = undefined


{-
Lambda Rule: "\" <Ident> "." <Expr> --> Lambda <Ident> <Expr>

Examples:
"\x.x" --> Lambda "x" (Var "x")
"  \arg1.  \arg2.      arg1  " --> Lambda "arg1" (Lambda "arg2" (Var "arg1"))

Hint: You already have an Expr parser above.
-}

parseLambda :: Parser Expr
parseLambda = undefined

{-
Applications Rule: "(" <Expr> "," <Expr> ")" --> Apply <Expr> <Expr>
<Apply Expr Expr> :
-}

parseApplication :: Parser Expr
parseApplication = undefined

{-
Now, we have defined all the necessary components to use the parseAndEval
function! You should play with the parse and parseAndEval functions with
different input strings to ensure that expressions parse/evaluate as expected.

As a convenience, we created a simple REPL
(https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) for you
so that you can interactively test out expressions.
-}

main :: IO ()
main = do
  line <- getLine
  parseAndEval line
  unless (line == "\n") main
