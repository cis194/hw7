{-
NOTE: This is just the stubbed version of HW6. You should replace this entire
file with your work from that homework.
-}

module LambdaCalc where

import Data.Maybe (fromMaybe)

{- 6.0 -- History of the Lambda Calculus -}

{-
In this assignment, you will be writing an evaluator for the lambda calculus!
Before we discuss the details of the lambda calculus let's briefly explore a
very fundamental question in computer science: how does one express
computation? This is a question that has been asked and answered by several
famous computer scientists since the dawn of computing. Having a formal
definition of computation is critical to designing algorithms and
answering questions about the limits of computation (take CIS 262 if this
topic interests you).

A well-known model of computation that may be familiar to you is the Turing
machine, devised by Alan Turing in 1936. At a very high level, the Turing
machine is a state-based model of computation that involves executing
expressions on an instruction tape and modifying some global state after the
evaluation of each expression. In this sense, it is not too hard to draw
similarities between programs in your favorite imperative programming language
and programs expressed using Turing machines. Programs in imperative
programming languages usually consist of a sequence of instructions to execute,
and after each expression, e.g. int x = 4;, the global state is modified. The
result of executing the program is really the final state after executing all
the expressions. Computation with Turing machines revolves around the
execution of expressions.

A different model of compuation, the lambda calculus (which we will explore in
this assignment), was devised by Alonzo Church around the same time that
Alan Turing devised the Turing machine. (Coincidentally, Church was Turing's
doctoral advisor.) If we think of Turing machines as the fundamental model
of computation for imperative programming languages, the lambda calculus can
be considered the basis of all functional programming languages.
-}


{- 6.1 -- The AST and Data Model -}

{-
The lambda calculus consists of the following terms:

VARIABLES are symbols whose concrete values will be determined during the
reduction process. They are immutable, much like the variables in Haskell.
Syntax-wise they are just identifiers, e.g. "x", "counter", "anyStringYouWant".

ABSTRACTIONS are similar to anonymous functions in Haskell. They have one
parameter and a body. Beyond serving as functions, abstractions play a second,
fundamental role in the lambda calculus. All data in the lambda calculus--
integers, booleans, lists, etc.-- is encoded using abstractions. Take a
minute to let this sink in. The most common encoding, and the one that we
will use in this assignment, is called the Church encoding. Abstractions are
denoted as `\x.M` where M is an arbitrary lambda expression (the body of the
abstraction) and x is an identifier that will be bound in M (the parameter).
We will refer to them interchangeably as abstractions and lambdas.

APPLICATION: Application is like function application in Haskell. Essentially
we reduce M until we get to an abstraction, then we bind the identifier from
that abstraction to the lambda expression N and reduce the body of the
abstraction. Application will be denoted as `(M, N)` where M and N are
arbitrary lambda expressions.

From the above descriptions, we arrive at the following custom type to
represent lambda calculus expressions in Haskell.
-}

type Ident =
  String

data Expr
  = Var Ident
  | Lambda Ident Expr
  | Apply Expr Expr
  deriving (Eq)


{- 6.2 -- Smart constructors -}

{-
Let's first define some "smart constructors" to make our life easier when
writing lambda calculus expressions. Smart constructors are a very common
idiom in Haskell for making a data model more convenient to work with while
maintaining a simple representation for your data.
-}

{-
These smart constructors basically make it simpler for us to create nested
abstractions and applications when dealing with functions of several
arguments (as in Haskell, abstractions in the lambda calculus are curried).
See how we use them below and play around with them to understand what
they do and how they work.
-}

var :: Ident -> Expr
var = Var

apply :: Expr -> [Expr] -> Expr
apply = foldl Apply

lambda :: [Ident] -> Expr -> Expr
lambda = flip $ foldr Lambda

{-
To simplify the conversion between Haskell values and their corresponding
Church encodings in the lambda calculus, we will also define smart
constructors for integers and booleans.
-}

{-
Under the Church encoding, we represent True as a function that accepts two
arguments and returns the first one. Similarly, False can be represented as
a function that accepts two arguments and returns the second one. The way
I like to think about this is that we are generalizing the boolean
constructors by making them arguments. In the functions that encode True
and False, we can think of the first argument as the constructor that
represents truth and the second argument as the constructor that represents
falsehood. The function that represents True will return the truth constructor
when evaluated, i.e. the first argument, while the function that represents
False will return the falsehood constructor when evaluated, i.e. the second
argument. Enough with the philosophy though!

The only weird thing to keep in mind though is that because values in the
lambda calculus are represented by abstractions, the entire function that
accepts two parameters and returns the first one is our representation of
true (rather than the result that that function evaluates to).
-}
bool :: Bool -> Expr
bool True = lambda ["t", "f"] (var "t")
bool False = undefined

{-
While we're at it, let's encode the natural numbers in the lambda calculus
using the same approach! If we think back to hw4, we remember that we can
encode the natural numbers using the following custom type.

    data Nat
      = Zero
      | OnePlus Nat

    zero  = Zero
    one   = OnePlus Zero
    two   = OnePlus (OnePlus Zero)
    three = ...

If we generalize the Zero and OnePlus constructors (much like how we
generalized the True and False constructors above), we arrive at
an encoding of the natural numbers called the Church numerals.
-}

zero, one, two :: Expr
zero = lambda ["s", "z"] (var "z")
one = lambda ["s", "z"] (apply (var "s") [var "z"])
two = lambda ["s", "z"] (apply (var "s") [apply (var "s") [var "z"]])

{-
Following the above pattern, we can write a recursive function that will
convert any arbitrary natural number in Haskell to its proper Church
encoding in the lambda calculus.
-}

int :: Integral a => a -> Expr
int haskellInt
  | haskellInt >= 0 = lambda ["s", "z"] (go haskellInt)
  | otherwise       = undefined -- for once it is OK to leave undefined here
  where
    go 0 = var "z"
    go n = apply (var "s") [go $ n - 1]

{- 6.2 Pretty Printing! -}

{-
If you mess around with lambda terms in a terminal, you'll notice that things
get pretty hard to read, pretty quickly. This calls for pretty printing via
a show instance for Expr!
-}



instance Show Expr where
  {-
  To ease in debugging we provide more readable representations for some
  common lambda expressions. Feel free to add any other expressions to this
  Show instance as you see fit.
  -}
  show (Lambda "x" (Var "x")) = "id"
  show (Lambda "s" (Lambda "z" (Var "z"))) = "zero"
  show (Lambda "t" (Lambda "f" (Var "t"))) = "true"
  show (Lambda "t" (Lambda "f" (Var "f"))) = "false"

  show (Var ident) = ident
  show (Apply fexpr@(Var _) xexpr@(Var _)) =
    show fexpr ++ " " ++ show xexpr
  show (Apply fexpr@(Var _) xexpr) =
    show fexpr ++ " " ++ parens (show xexpr)
  show (Apply fexpr xexpr@(Var _)) =
    parens (show fexpr) ++ " " ++ show xexpr
  show (Apply fexpr xexpr) =
    parens (show fexpr) ++ " " ++ parens (show xexpr)
  show (Lambda ident expr@(Lambda _ _)) =
    "\\" ++ ident ++ ". " ++ parens (show expr)
  show (Lambda ident expr) =
    "\\" ++ ident ++ ". " ++ show expr

parens :: String -> String
parens s = "(" ++ s ++ ")"

{- 6.3 Computing with the Lambda Calculus -}

{-
Now that we have an encoding of some basic data types, we can define
some practical functions that operate on these data types! As CIS 194
is a course about Haskell and not the lambda calculus, don't bother
trying to understand these definitions unless you really, really want to.

We append `E` to the names of the expressions below both to indicate to
ourselves that these are Exprs and to disambiguate them from
the Haskell functions of the same name.
-}

notE, andE, orE, succE, idE :: Expr
notE = lambda ["b"] (apply (var "b") [bool False, bool True])
andE = lambda ["b", "c"] (apply (var "b") [var "c", var "b"])
orE = lambda ["b", "c"] (apply (var "b") [var "b", var "c"])
idE = lambda ["x"] (var "x")
succE = lambda ["n", "s", "z"] $
  apply (var "s") [apply (apply (var "n") [var "s"]) [var "z"]]
plusE = lambda ["m", "n", "s", "z"] $
  apply (var "m") [var "s", apply (var "n") [var "s", var "z"]]


{- 6.5 Evaluation of the Lambda Caclulus -}

{-
Now we can create all sorts of arbitrary terms in the lamda calculus
(i.e: "\x.x", 'y', "( \x.x , \y.y )", "\x.\y.(y,x)" ). But how can we compute
with them? Remember from above that values in the lambda calculus are
actually abstractions? That informs our notion of how to evaluate lambda
expressions. Essentially, we use "rewrite rules" to reduce an arbitrarily
complex lambda expression until we are left with just an abstraction.
At that point, we can interpret the abstraction as an integer or a boolean or
anything else based on an encoding we choose beforehand.

In this assignment, we will focus on one of these rewriting rules called
beta reduction, which works as follows:

Given a lambda expression L = Lambda "x" E (where E is an arbitrary
lambda expression of type Expr) and an arbitrary lambda expression S,
beta-reduction on (L, S) results in a new lambda expression E' that is
equivalent to E except for the fact that we substitute all occurences of
x in E with the value S.

Examples:
----------
* Beta reduction of (\x.x, y) will yield y, because we substitute
  the value of y in for x in the body of the lambda.

* Likewise, beta reduction of (\x.\y.(x,y), \z.z) yields \y.(\z.z,y)
  because we just substitute \z.z for x in the body of the lambda.
  Note that the body of the lambda can be an arbitrary lambda expression.
----------

In Haskell, the way we will reduce terms in the lambda calculus is by storing
an enviroment that maps the variables that are in scope to the corresponding
expression that they should be replaced with:
-}

type Env = [(Ident, Expr)]

{-
In this sense, each lambda calculus expression has an associated environment
that maps variables in the current expression to other lambda
calculus expressions.
-}


{-
If we encounter a variable that is not in scope during our reduction,
we will use the following type to report which variable was not
in scope so that we can output that information for debugging.
-}
newtype RuntimeError = NotInScope Ident
  deriving (Eq, Show)

{-
Our reducer will take in a lambda calculus expression to reduce, an environment
of the type defined above, and return a fully reduced lambda expression,
which is a Lambda with an associated environment. If at any point in the
reduction we encounter an expression Var ident where ident is not bound
(i.e, not in the environment), we will consider this a runtime error
and the reduction should fail by using the Left constructor.
This is similar to referencing a variable that you never declared in an
imperative programming language.

Before we leave you to write the reducer (long intro, I know), let's
specify some guidelines for the behavior of our reducer that will
inform our implementation.

1. Function arguments should be reduced as much as possible before being
"passed into" the body of the abstraction that is applied to them.

2. Beta reduction should not do anything to terms of the form
Lambda "x" E. This is because these terms are actually the values of
the language. The same way that there is no way to reduce the value
3 in Java (or insert your favorite imperative language here) since it
is a literal, you can't reduce the value \s.\z.(s, (s, (s, z))) in the
lambda calculus since it represents the literal value 3.

3. Make sure that you handle closures properly. Suppose I am reducing the
lambda expression ((\x. (\y. x), true), false). x should be bound to true
and y should be bound to false in which case we expect to get back true.
If you don't handle closures properly though, you will forget the
information that x is true when reducing the outer apply which will
cause a NotInScope error.
-}

reduce :: Expr -> Env -> Either RuntimeError (Expr, Env)
reduce = undefined


{-
For ease, we can define an evaluation function, which calls the reducer
above with a default initial enviroment containing some the lambda
expressions we defined above. It prints the reduced expression.
It also does one final substitution of all the variables in the final
enviroment to make things look better for printing. You should test
your implementation in GHCi using this function. Here are some tests to
get you started:

    eval $ apply (var "and") [bool False, bool True]
    eval $ apply (var "id") [int 2]
    eval $ apply (apply (lambda ["x", "y"] (var "x")) [bool True]) [bool False]
-}

eval :: Expr -> IO ()
eval expr =
  case reduce expr defaultEnv of
    Left err -> putStrLn $ "error: " ++ show err
    Right (expr', env) -> print $ subst expr' env
  where
    defaultEnv =
      [ ("id", idE)
      , ("succ", succE)
      , ("plus", plusE)
      , ("not", notE)
      , ("and", andE)
      , ("or", orE)
      ]
    subst (Var ident) env = fromMaybe (Var ident) $ lookup ident env
    subst (Apply fexpr xexpr) env = Apply (subst fexpr env) (subst xexpr env)
    subst (Lambda ident fexpr) env = Lambda ident (subst fexpr env')
      where env' = filter (\(i, _) -> ident /= i) env

{-
Woohoo! Now things look a bit better at the terminal. Congratulations, you
just defined a lambda calculus representation along with an evaluator in
Haskell! If you want to learn more about this, check out System F
(or take CIS 500 to learn more about programming with types).
-}
