# {#title}

<h1>
    Beginning Haskell
</h1>
<h3>
    Bob Ippolito (<a href="https://twitter.com/etrepum">@etrepum</a>)<br>
    BayHac 2014
</h3>
<h4>
[bob.ippoli.to/beginning-haskell-bayhac-2014]
</h4>
<br><br>
<h4><strong>Now is a good time to install GHC</strong><br>
<br>[bit.ly/install-ghc](http://bit.ly/install-ghc)</h4>

# Who am I?

- Haskell user since 2012
  (ported [exercism.io](http://exercism.io) curriculum)
- Spending most of my time with
  [Mission Bit](http://www.missionbit.com/), teaching
  after school coding classes (but not in Haskell… yet)
- Doing a bit of advising/investing in startups

# Haskell's Appeal

- Abstractions can often be used without penalty
- Efficient serial, parallel and concurrent programming
- Type system makes maintenance easier
- Nice syntax (not too heavy or lightweight)
- Fantastic community & ecosystem

# My stumbling blocks

- So `.` many `<$>` operators `>>=`, many `$` without `<*>` names `!!`
- Types took some getting used to
- Non-strict evaluation didn't match my intuition/experience
- Loads of new terminology

# Use the Hoogle

* [haskell.org/hoogle](http://www.haskell.org/hoogle/)
* Search for [Prelude](http://www.haskell.org/hoogle/?hoogle=Prelude)
* The [Prelude module](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html)
  contains all of the built-in functions, types, and typeclasses
* Most of Haskell is written in Haskell, use the source links!

# Haskell Syntax

Types
~   Defines types and typeclasses
~   Constructors and record accessors become terms

Terms
~   Named bindings
~   Instances of constructors
~   Functions
~   Control flow

# Types

* Examples: `Bool`, `Int`, `[a]`, `Maybe a`
* Start with a capital letter
* Defined with `data` or `newtype`
* Aliases made with `type`

# Sum Types {.big-code}

* Sum types enumerate all possible inhabitants
* Bool has 2 possibilities, Ordering has 3, …

```haskell
data Bool = True | False

data Ordering = LT | EQ | GT

data Choice = Definitely | Possibly | NoWay

data Int = … | -1 | 0 | 1 | 2 | …

data Char = … | 'a' | 'b' | …
```

# Product Types {.big-code}

* Product types are like structs, with fields that contain other types
* Choices has `3 * 3 == 9` possible inhabitants
* Can name these fields using record syntax (defines getters automatically)

```haskell
data CoinFlip = CoinFlip Bool

data Choices = Choices Choice Choice

data Coord = Coord { x :: Double, y :: Double }
```

# Sum of Products {.big-code}

* Possibly has `(1 * 2) + 1` possibilities

```haskell
data Possibly = Certainly Bool
              | Uncertain

data DrawCommand = Point Int Int
                 | Line Int Int Int Int
				 | Rect Int Int Int Int

data IntTree = Node Int IntTree IntTree
	         | Leaf
```

# Abstract Data Types {.big-code}

* Type variables are lowercase
* `type` creates aliases (can help readability)

```haskell
data List a = Cons a (List a)
            | Nil

data Maybe a = Just a
	         | Nothing

type IntList = List Int
type MaybeBool = Maybe Bool
type String = [Char]
```

# Special Type Syntax {.big-code}

* Unit, Tuples, Lists, and Functions have special syntax
* Can also be written in prefix form

```haskell
type Unit = ()

type ListOfInt = [Int]
type ListOfInt = [] Int

type AddFun = Int -> Int -> Int
type AddFun = Int -> (Int -> Int)
type AddFun = (->) Int ((->) Int Int)

type IntTuple = (Int, Int)
type IntTuple = (,) Int Int
```

# {#types-and-constructors-1 .small-title .big-code .highlight-type}

<h1><span class="hl-type">Types</span> and <span class="hl-constructor">Constructors</span></h1>

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt hl-type">Choice</span> <span class="fu">=</span> <span class="dt hl-constructor">Definitely</span>
            <span class="fu">|</span> <span class="dt hl-constructor">Possibly</span>
            <span class="fu">|</span> <span class="dt hl-constructor">NoWay</span>

<span class="kw">data</span> <span class="dt hl-type">Choices</span> <span class="fu">=</span> <span class="dt hl-constructor">Choices</span> <span class="dt hl-type">Choice</span> <span class="dt hl-type">Choice</span>

<span class="ot">mkChoices ::</span> <span class="dt hl-type">Choice</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choice</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choices</span>
mkChoices a b <span class="fu">=</span> <span class="dt hl-constructor">Choices</span> a b

<span class="ot">fstChoice ::</span> <span class="dt hl-type">Choices</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choice</span>
fstChoice (<span class="dt hl-constructor">Choices</span> a _) <span class="fu">=</span> a</code></pre>

# {#types-and-constructors-2 .small-title .big-code .highlight-constructor}

<h1><span class="hl-type">Types</span> and <span class="hl-constructor">Constructors</span></h1>

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt hl-type">Choice</span> <span class="fu">=</span> <span class="dt hl-constructor">Definitely</span>
            <span class="fu">|</span> <span class="dt hl-constructor">Possibly</span>
            <span class="fu">|</span> <span class="dt hl-constructor">NoWay</span>

<span class="kw">data</span> <span class="dt hl-type">Choices</span> <span class="fu">=</span> <span class="dt hl-constructor">Choices</span> <span class="dt hl-type">Choice</span> <span class="dt hl-type">Choice</span>

<span class="ot">mkChoices ::</span> <span class="dt hl-type">Choice</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choice</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choices</span>
mkChoices a b <span class="fu">=</span> <span class="dt hl-constructor">Choices</span> a b

<span class="ot">fstChoice ::</span> <span class="dt hl-type">Choices</span> <span class="ot">-&gt;</span> <span class="dt hl-type">Choice</span>
fstChoice (<span class="dt hl-constructor">Choices</span> a _) <span class="fu">=</span> a</code></pre>

# Using Types {.big-code}

```haskell
-- Terms can be annotated in-line
2 ^ (1 :: Int)

-- Bindings can be annotated
success :: a -> Maybe a
-- Constructors are terms
-- (and product constructors are functions)
success x = Just x

-- Constructors can be pattern matched
-- _ is a wildcard
case success True of
  Just True -> ()
  _         -> ()
```

# GHCi

<h2>Interactive Haskell</h2>

# {#runhaskell .medium-code}

```bash
$ runhaskell --help
Usage: runghc [runghc flags] [GHC flags] module [program args]

The runghc flags are
    -f /path/to/ghc       Tell runghc where GHC is
    --help                Print this usage information
    --version             Print version number
```

# {#ghci-start .medium-code}

```
$ ghci
GHCi, version 7.8.2: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
h> 
```

# {#ghci-t .big-code}

<h2>
`:t` shows type information</h2>
```haskell
h> :t map
map :: (a -> b) -> [a] -> [b]
h> :t map (+1)
map (+1) :: Num b => [b] -> [b]
h> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

# {#ghci-i-typeclass .big-code}

<h2>`:i` shows typeclass info</h2>

```haskell
h> :i Num
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
    -- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
instance Num Float -- Defined in `GHC.Float'
instance Num Double -- Defined in `GHC.Float'
```

# {#ghci-i-value .big-code}

<h2>`:i` shows term info</h2>

```haskell
h> :info map
map :: (a -> b) -> [a] -> [b]   
-- Defined in `GHC.Base'
h> :info (>>=)
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  ...
  	-- Defined in `GHC.Base'
infixl 1 >>=
```

# {#ghci-i-type .big-code}

<h2>`:i` shows type info</h2>

```haskell
h> :info Int
data Int = ghc-prim:GHC.Types.I#
  ghc-prim:GHC.Prim.Int#
  -- Defined in `ghc-prim:GHC.Types'
instance Bounded Int -- Defined in `GHC.Enum'
instance Enum Int -- Defined in `GHC.Enum'
instance Eq Int -- Defined in `GHC.Classes'
instance Integral Int -- Defined in `GHC.Real'
instance Num Int -- Defined in `GHC.Num'
instance Ord Int -- Defined in `GHC.Classes'
instance Read Int -- Defined in `GHC.Read'
instance Real Int -- Defined in `GHC.Real'
instance Show Int -- Defined in `GHC.Show'
```

# {#ghci-load-reload .big-code}

<h2>`:l` load a module</h2>
<h2>`:r` to reload</h2>

```haskell
h> :! echo 'hello = print "hello"' > Hello.hs
h> :l Hello
[1 of 1] Compiling Main ( Hello.hs, interpreted )
Ok, modules loaded: Main.
h> hello
"hello"
h> :! echo 'hello = print "HELLO"' > Hello.hs
h> :r
[1 of 1] Compiling Main ( Hello.hs, interpreted )
Ok, modules loaded: Main.
h> hello
"HELLO"
```

# map {.big-code}

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

# foldr {.big-code}

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
   where
     go []     = z
     go (y:ys) = y `k` go ys
```

# Pattern Matching {.big-code}

```haskell
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
```

# Pattern Matching {.big-code}

Haskell only implements linear patterns

```haskell
-- DOES NOT WORK!
isEqual :: a -> a -> Bool
isEqual a a = True
isEqual _ _ = False
```

<blockquote>
This isn't even possible!
Only constructors can be pattern matched.
Types have no built-in equality.
</blockquote>

# &#96;Infix&#96; and (Prefix) {.big-code}

```haskell
-- Symbolic operators can be used
-- prefix when in (parentheses)
(+) a b

-- Named functions can be used
-- infix when in `backticks`
x `elem` xs

-- infixl, infixr define associativity
-- and precedence (0 lowest, 9 highest)
infixr 5 `append`
a `append` b = a ++ b

```

# Functions & Lambdas {.big-code}

```haskell
add :: Integer -> Integer -> Integer
add acc x = acc + x

sumFun :: [Integer] -> Integer
sumFun xs = foldl add 0 xs

sumLambda :: [Integer] -> Integer
sumLambda xs = foldl (\acc x -> acc + x) 0 xs
```

# Functions & Lambdas {.big-code}

* Haskell *only* has functions of one argument
* `a -> b -> c` is really `a -> (b -> c)`
* `f a b` is really `(f a) b`
* Let's leverage that&hellip;

# Functions & Lambdas {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">add ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span>
add acc x <span class="fu">=</span> acc <span class="fu">+</span> x

<span class="ot">sumFun ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumFun <span class="hl">xs</span> <span class="fu">=</span> foldl add <span class="dv">0</span> <span class="hl">xs</span>

<span class="ot">sumLambda ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumLambda <span class="hl">xs</span> <span class="fu">=</span> foldl (\acc x <span class="ot">-&gt;</span> acc <span class="fu">+</span> x) <span class="dv">0</span> <span class="hl">xs</span></code></pre>

# Functions & Lambdas {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">add ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span>
add acc x <span class="fu">=</span> acc <span class="fu hl">+</span> x

<span class="ot">sumFun ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumFun <span class="fu">=</span> foldl add <span class="dv">0</span>

<span class="ot">sumLambda ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumLambda <span class="fu">=</span> foldl (\acc x <span class="ot">-&gt;</span> acc <span class="fu hl">+</span> x) <span class="dv">0</span></code></pre>

# Functions & Lambdas {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">add ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span>
add acc <span class="hl">x</span> <span class="fu">=</span> <span class="fu">(+)</span> acc <span class="hl">x</span>

<span class="ot">sumFun ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumFun <span class="fu">=</span> foldl add <span class="dv">0</span>

<span class="ot">sumLambda ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumLambda <span class="fu">=</span> foldl (\acc <span class="hl">x</span> <span class="ot">-&gt;</span> <span class="fu">(+)</span> acc <span class="hl">x</span>) <span class="dv">0</span></code></pre>

# Functions & Lambdas {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">add ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span>
add <span class="hl">acc</span> <span class="fu">=</span> <span class="fu">(+)</span> <span class="hl">acc</span>

<span class="ot">sumFun ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumFun <span class="fu">=</span> foldl add <span class="dv">0</span>

<span class="ot">sumLambda ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumLambda <span class="fu">=</span> foldl (\<span class="hl">acc</span> <span class="ot">-&gt;</span> <span class="fu">(+)</span> <span class="hl">acc</span>) <span class="dv">0</span></code></pre>

# Functions & Lambdas {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">add ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span>
add <span class="fu">=</span> <span class="fu">(+)</span>

<span class="ot">sumFun ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumFun <span class="fu">=</span> foldl add <span class="dv">0</span>

<span class="ot">sumLambda ::</span> [<span class="dt">Integer</span>] <span class="ot">-&gt;</span> <span class="dt">Integer</span>
sumLambda <span class="fu">=</span> foldl <span class="fu">(+)</span> <span class="dv">0</span></code></pre>

# Guards {.big-code}

```haskell
isNegative :: (Num a) => a -> Bool
isNegative x
  | x < 0     = True
  | otherwise = False

absoluteValue :: (Num a) => a -> Bool
absoluteValue x
  | isNegative x = -x
  | otherwise    = x
```

# Built-in types {.big-code .small-title}

```haskell
-- (), pronounced "unit"
unit :: ()
unit = ()

-- Char
someChar :: Char
someChar = 'x'

-- Instances of Num typeclass
someDouble :: Double
someDouble = 1

-- Instances of Fractional typeclass
someRatio :: Rational
someRatio = 1.2345
```

# Lists & Tuples {.big-code .small-title}

```haskell
-- [a], type can be written prefix as `[] a`
someList, someOtherList :: [Int]
someList = [1, 2, 3]
someOtherList = 4 : 5 : 6 : []
dontWriteThis = (:) 4 (5 : (:) 6 [])

-- (a, b), can be written prefix as `(,) a b`
someTuple, someOtherTuple :: (Int, Char)
someTuple = (10, '4')
someOtherTuple = (,) 4 '2'

-- [Char], also known as String
-- (also see the OverloadedStrings extension)
someString :: String
someString = "foo"
```

# Challenge?

Finish this dice game (see TODO):
<br><br>
<h1>[lpaste.net/104237](http://lpaste.net/104237)</h1>

# Typeclass Syntax {.big-code}

```haskell
class Equals a where
  isEqual :: a -> a -> Bool

instance Equals Choice where
  isEqual Definitely Definitely = True
  isEqual Possibly   Possibly   = True
  isEqual NoWay      NoWay      = True
  isEqual _          _          = False

instance (Equals a) => Equals [a] where
  isEqual (a:as) (b:bs) = isEqual a b &&
                          isEqual as bs
  isEqual as     bs     = null as && null bs
```

# Typeclass Syntax {.big-code}

```haskell
{-
class Eq a where
  (==) :: a -> a -> Bool
-}

instance Eq Choice where
  Definitely == Definitely = True
  Possibly   == Possibly   = True
  NoWay      == NoWay      = True
  _          == _          = False
```

# Typeclass Syntax {.big-code}

```haskell
data Choice = Definitely
            | Possibly
            | NoWay
            deriving (Eq)
```

# Typeclass Syntax {.big-code}

```haskell
data Choice = Definitely
            | Possibly
            | NoWay
            deriving ( Eq, Ord, Enum, Bounded
                     , Show, Read )
```


# QuickCheck {.big-code}

```haskell
prop_intIdentity :: Int -> Bool
prop_intIdentity i = i == i
```

# QuickCheck {.big-code}

```bash
$ ghci
```
```haskell
λ> import Test.QuickCheck
λ> quickCheck (\i -> (i :: Int) == i)
+++ OK, passed 100 tests.
```

# QuickCheck Isn't Magic {.big-code .small-title}

```haskell
λ> import Test.QuickCheck
λ> quickCheck (\i -> (i :: Double) + 1 > i)
+++ OK, passed 100 tests.
```

# QuickCheck Isn't Magic {.big-code .small-title}

```haskell
λ> import Test.QuickCheck
λ> quickCheck (\i -> (i :: Double) + 1 > i)
+++ OK, passed 100 tests.
λ> let i = 0/0 :: Double in i + 1 > i
False
```

# QuickCheck Isn't Magic {.big-code .small-title}

```haskell
λ> import Test.QuickCheck
λ> quickCheck (\i -> (i :: Double) + 1 > i)
+++ OK, passed 100 tests.
λ> let i = 0/0 :: Double in i + 1 > i
False
λ> let i = 1e16 :: Double in i + 1 > i
False
```

# Do syntax (IO) {.big-code}

```haskell
main :: IO ()
main = do
  secret <- readFile "/etc/passwd"
  writeFile "/tmp/passwd" secret
  return ()
```

# Do syntax {.big-code}

```haskell
do m
-- desugars to:
m

do a <- m
   return a
-- desugars to:
m >>= \a -> return a

do m
   return ()
-- desugars to:
m >> return ()
```

# Do syntax (IO) {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span> <span class="hl"><span class="kw">do</span></span>
  <span class="hl">secret <span class="ot">&lt;-</span></span> readFile <span class="st">"/etc/passwd"</span>
  writeFile <span class="st">"/tmp/passwd"</span> secret
  return ()</code></pre>

# Do syntax (IO) {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span>
  readFile <span class="st">"/etc/passwd"</span> <span class="hl">&gt;&gt;= \secret -> do</span>
  writeFile <span class="st">"/tmp/passwd"</span> secret
  return ()</code></pre>

# Do syntax (IO) {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span>
  readFile <span class="st">"/etc/passwd"</span> <span class="fu">&gt;&gt;=</span> \secret <span class="ot">-></span>
  writeFile <span class="st">"/tmp/passwd"</span> secret <span class="hl">&gt;&gt;</span>
  return ()</code></pre>

# Do syntax (IO) {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span>
  readFile <span class="st">"/etc/passwd"</span> <span class="fu">&gt;&gt;=</span><span class="hl"> \secret <span class="ot">-></span></span>
  writeFile <span class="st">"/tmp/passwd"</span> <span class="hl">secret</span></code></pre>

# Do syntax (IO) {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span>
  readFile <span class="st">"/etc/passwd"</span> <span class="fu">&gt;&gt;=</span>
  writeFile <span class="st">"/tmp/passwd"</span></code></pre>

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = [ y | x <- xs, y <- f x ]
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = do
  x <- xs
  y <- f x
  return y
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = do
  x <- xs
  f x
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = xs >>= \x -> f x
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = xs >>= f
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = flip (>>=) f xs
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap = flip (>>=)
```

# Do syntax ([a]) {.big-code}

```haskell
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap = (=<<)
```

# {#side-effects .big-code}

```haskell
-- WordCount1.hs

main :: IO ()
main = do
  input <- getContents
  let wordCount = length (words input)
  print wordCount
```

# {#side-effects-2 .big-code}

```haskell
-- WordCount2.hs

main :: IO ()
main =
  getContents >>= \input ->
    let wordCount = length (words input)
    in print wordCount
```

# {#side-effects-3 .big-code}

```haskell
-- WordCount3.hs

main :: IO ()
main = getContents >>= print . length . words
```

# what.the `>>=`?

* `do` is just syntax sugar for the `>>=` (bind) operator.
* IO is still purely functional, we are just building a graph
  of actions, *not* executing them in-place!
* Starting from `main`, the Haskell runtime will *evaluate* these actions
* It works much like continuation passing style, with a state
  variable for the current world state (behind the scenes)
* There are ways to cheat and write code that is not pure, but you
  will have to go out of your way to do it

# Common combinators {.big-code}

```haskell
-- Function composition
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- Function application (with a lower precedence)
($) :: (a -> b) -> a -> b
f $ x =  f x
```

# Pure

* Haskell's purity implies referential transparency
* This means that function invocation can be freely replaced with its
  return value without changing semantics
* Fantastic for optimizations
* Also enables equational reasoning, which makes it easier to prove
  code correct

# {#compiler}

<!--
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain
-->
<svg viewBox="0 0 1000 1000" class="full diagram">
  <defs>
    <marker id="Triangle"
      viewBox="0 0 10 10" refX="0" refY="5" 
      markerUnits="strokeWidth"
      markerWidth="4" markerHeight="3"
      orient="auto">
      <path d="M 0 0 L 10 5 L 0 10 z" />
    </marker>
  </defs>
  <g class="right-title" transform="translate(1000, 20)">
    <text>GHC compilation phases</text>
  </g>
  <g class="phase parse" transform="translate(500, 85)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>Parse</text>
  </g>
  <g class="phase rename" transform="translate(500, 215)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>Rename</text>
  </g>
  <g class="phase typecheck" transform="translate(500, 345)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>Typecheck</text>
  </g>
  <g class="phase desugar" transform="translate(500, 475)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>Desugar</text>
  </g>
  <g class="phase optimize" transform="translate(500, 605)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <path d="M 65,35 a 160,80 0 1,0 40,-80" marker-end="url(#Triangle)"/>
    <text x="220" class="outside">Core</text>
    <ellipse rx="120" ry="35"/>
    <text>Optimize</text>
  </g>
  <g class="phase codegen" transform="translate(500, 735)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>Code gen</text>
  </g>
  <g class="phase llvm" transform="translate(500, 865)">
    <line y1="-85" y2="-65" marker-end="url(#Triangle)" />
    <ellipse rx="120" ry="35"/>
    <text>LLVM</text>
    <line y1="45" y2="65" marker-end="url(#Triangle)" />
  </g>
</svg>

# Optimizations

<!--
http://stackoverflow.com/questions/12653787/what-optimizations-can-ghc-be-expected-to-perform-reliably 
http://research.microsoft.com/en-us/um/people/simonpj/papers/spec-constr/spec-constr.pdf
http://www.haskell.org/ghc/docs/latest/html/users_guide/options-optimise.html
-->

* Common sub-expression elimination
* Inlining (cross-module too!)
* Specialize
* Float out
* Float inwards
* Demand analysis
* Worker/Wrapper binds
* Liberate case
* Call-pattern specialization (SpecConstr)

# GHC RULES!
<!--
http://www.haskell.org/haskellwiki/Playing_by_the_rules
http://www.haskell.org/haskellwiki/GHC/Using_rules
https://ghc.haskell.org/trac/ghc/wiki/RewriteRules
-->

* Term rewriting engine
* RULES pragma allows *library defined optimizations*
* Used to great effect for short cut fusion
* Example: `map f (map g xs) = map (f . g) xs`
* Prevent building of intermediate data structures
* Commonly used for lists, Text, ByteString, etc.
* Great incentive to write high-level code!
* ANY LIBRARY CAN USE THIS!

# {#ghc-rules-ex .big-code}

```haskell
{-# RULES
"ByteString specialise break (x==)" forall x.
    break ((==) x) = breakByte x
"ByteString specialise break (==x)" forall x.
    break (==x) = breakByte x
  #-}
```

# GHC RULES {.big-code}

```haskell
{-# RULES
"ByteString specialise break (x==)" forall x.
    break ((==) x) = breakByte x
"ByteString specialise break (==x)" forall x.
    break (==x) = breakByte x
  #-}

import Data.ByteString.Char8 (ByteString, break)

splitLine :: ByteString -> (ByteString, ByteString)
splitLine = break (=='\n')
```

# GHC RULES {.big-code}

```haskell
{-# RULES
"ByteString specialise break (x==)" forall x.
    break ((==) x) = breakByte x
"ByteString specialise break (==x)" forall x.
    break (==x) = breakByte x
  #-}

import Data.ByteString.Char8 (ByteString, break)

splitLine :: ByteString -> (ByteString, ByteString)
splitLine = breakByte '\n'
```

# Lazy

* Call by need (outside in), not call by value (inside out)
* Non-strict evaluation separates equation from execution
* No need for special forms for control flow, no value restriction
* Enables infinite or cyclic data structures
* Can skip unused computation (better minimum bounds)

# {#lazy-ramsey}

![lazy](img/ramsey-lazy-2013.jpg)


# Call by need

<!--
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode
http://research.microsoft.com/apps/pubs/default.aspx?id=67083
-->

* Expressions are translated into a graph (not a tree!)
* Evaluated with STG (Spineless Tagless G-Machine)
* Pattern matching forces evaluation

# Non-Strict Evaluation {.big-code}

```haskell
-- [1..] is an infinite list, [1, 2, 3, ...]
print (head (map (*2) [1..]))
```

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- [1..] is an infinite list, [1, 2, 3, ...]</span>
<span class="hl">print (</span>head (map (<span class="fu">*</span><span class="dv">2</span>) [<span class="dv">1</span><span class="fu">..</span>])<span class="hl">)</span>
<span class="co">-- Outside in, print x = putStrLn (show x)</span>
<span class="hl">putStrLn (show (</span>head (map (<span class="fu">*</span><span class="dv">2</span>) [<span class="dv">1</span><span class="fu">..</span>]<span class="hl">))</span></code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- Outside in, print x = putStrLn (show x)</span>
putStrLn (show (<span class="hl2">head (<span class="hl1">map (<span class="fu">*</span><span class="dv">2</span>) <span class="hl">[<span class="dv">1</span><span class="fu">..</span>]</span>)</span>)</span>
<span class="co">-- head (x:_) = x</span>
<span class="co">-- map f (x:xs) = f x : map f xs</span>
<span class="co">-- desugar [1..] syntax</span>
putStrLn (show (head (map (<span class="fu">*</span><span class="dv">2</span>) (<span class="hl">enumFrom <span class="dv">1</span></span>))))</code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- desugar [1..] syntax</span>
putStrLn (show (head (map (<span class="fu">*</span><span class="dv">2</span>) (<span class="hl">enumFrom <span class="dv">1</span></span>))))
<span class="co">-- enumFrom n = n : enumFrom (succ n)</span>
putStrLn (show (head (map (<span class="fu">*</span><span class="dv">2</span>)
                          (<span class="hl"><span class="dv">1</span> <span class="fu">:</span> enumFrom (succ <span class="dv">1</span>)</span>))))</code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- enumFrom n = n : enumFrom (succ n)</span>
putStrLn (show (head (<span class="hl1">map (<span class="fu">*</span><span class="dv">2</span>)</span>
                          <span class="hl1">(<span class="hl"><span class="dv">1</span> <span class="fu">:</span></span> enumFrom (succ <span class="dv">1</span>))</span>)))
<span class="co">-- apply map</span>
putStrLn (show (head
                  (<span class="hl">(<span class="dv">1</span><span class="fu">*</span><span class="dv">2</span>) <span class="fu">:</span></span>
                   <span class="hl1">map (<span class="fu">*</span><span class="dv">2</span>) (enumFrom (succ <span class="dv">1</span>))</span>)))</code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- apply map</span>
putStrLn (show (<span class="hl1">head (<span class="hl">(<span class="dv">1</span><span class="fu">*</span><span class="dv">2</span>)</span> <span class="fu">:</span> …)</span>))
<span class="co">-- apply head</span>
putStrLn (show <span class="hl">(<span class="dv">1</span><span class="fu">*</span><span class="dv">2</span>)</span>)</code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- apply head</span>
putStrLn (show <span class="hl">(<span class="dv">1</span><span class="fu">*</span><span class="dv">2</span>)</span>)
<span class="co">-- show pattern matches on its argument</span>
putStrLn (show <span class="hl"><span class="dv">2</span></span>)</code></pre>

# Non-Strict Evaluation {.big-code .highlight}

<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="co">-- show pattern matches on its argument</span>
putStrLn (<span class="hl">show <span class="dv">2</span></span>)
<span class="co">-- apply show</span>
putStrLn <span class="hl"><span class="st">"2"</span></span></code></pre>

# {#control-flow .big-code}

```haskell
if' :: Bool -> a -> a -> a
if' cond a b = case cond of
  True  -> a
  False -> b

(&&) :: Bool -> Bool -> Bool
a && b = case a of
  True  -> b
  False -> False

const :: a -> b -> a
const x = \_ -> x
```

# {#infinite-programming .big-code}

```haskell
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []
```

# Types

* Enforce constraints at compile time
* No NULL
* Can have parametric polymorphism and/or recursion
* Built-in types are not special (other than syntax)
* Typeclasses for *ad hoc* polymorphism (overloading)

# {#constraints .medium-code}

```haskell
h> let f x = head True

<interactive>:23:16:
    Couldn't match expected type `[a0]' with actual type `Bool'
    In the first argument of `head', namely `True'
    In the expression: head True
    In an equation for `f': f x = head True
```

```haskell
h> let f x = heads True

<interactive>:24:11:
    Not in scope: `heads'
    Perhaps you meant one of these:
      `reads' (imported from Prelude),
	  `head' (imported from Prelude)
```

# {#bottoms .medium-code}

```haskell
h> let x = x in x
-- Infinite recursion, not a fun case to deal with!

h> case False of True -> ()
*** Exception: <interactive>:29:1-24: Non-exhaustive patterns …

h> head []
*** Exception: Prelude.head: empty list

h> error "this throws an exception"
*** Exception: this throws an exception

h> undefined
*** Exception: Prelude.undefined
```

# {#polymorphic .medium-code}

```haskell
-- Polymorphic and recursive
data List a = Cons a (List a)
            | Nil
            deriving (Show)

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show)

listMap :: (a -> b) -> List a -> List b
listMap _ Nil         = Nil
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

treeToList :: Tree a -> List a
treeToList root = go root Nil
  where
    -- Note that `go` returns a function!
    go (Leaf x)     = Cons x
    go (Branch l r) = go l . go r
```

# Typeclasses

* Used for many of the Prelude operators and numeric literals
* Ad hoc polymorphism (overloading)
* Many built-in typeclasses can be automatically derived
  (Eq, Ord, Enum, Bounded, Show, and Read)!

# {#typeclass-example .big-code}

```haskell
module List where

data List a = Cons a (List a)
            | Nil

instance (Eq a) => Eq (List a) where
  (Cons a as) == (Cons b bs) = a == b && as == bs
  Nil         == Nil         = True
  _           == _           = False

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

```

# {#typeclass-example-2 .big-code}

<!-- http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html -->
```haskell
{-# LANGUAGE DeriveFunctor #-}

module List where

data List a = Cons a (List a)
            | Nil
            deriving (Eq, Functor)

```

# {#newtype .big-code}

```haskell
import Data.List (sort)

newtype Down a = Down { unDown :: a }
                 deriving (Eq)

instance (Ord a) => Ord (Down a) where
  compare (Down a) (Down b) = case compare a b of
    LT -> GT
    EQ -> EQ
    GT -> LT

reverseSort :: Ord a => [a] -> [a]
reverseSort = map unDown . sort . map Down

```

# Abstractions

Monoid
~   Has an identity and an associative operation
Functor
~   Anything that can be mapped over (preserving structure)
Applicative
~   Functor, but can apply function from inside
Monad
~   Applicative, but can return any structure

# Monoid {.big-code}

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid [a] where
  mempty = []
  mappend = (++)

infixr 6 <>
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend
```

# Functor {.big-code}

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ Nothing  = Nothing

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

# Applicative {.big-code}

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
  pure x = [x]
  fs <*> xs = concatMap (\f -> map f xs) fs

instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)
  _      <*> _      = Nothing
```

# Monad {.big-code}

```haskell
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>)  :: m a -> m b -> m b
  ma >> mb = ma >>= \_ -> mb

instance Monad [] where
  return = pure
  m >>= f = concatMap f m

instance Monad Maybe where
  return = pure
  Just x  >>= f = f x
  Nothing >>= _ = Nothing
```

# Parsing

```haskell
{-# LANGUAGE OverloadedStrings #-}
module SJSON where
import Prelude hiding (concat)
import Data.Text (Text, concat)
import Data.Attoparsec.Text
import Control.Applicative

data JSON = JArray [JSON]
          | JObject [(Text, JSON)]
          | JText Text
          deriving (Show)

pJSON :: Parser JSON
pJSON = choice [ pText, pObject, pArray ]
  where
    pString = concat <$> "\"" .*> many pStringChunk <*. "\""
    pStringChunk = choice [ "\\\"" .*> pure "\""
                          , takeWhile1 (not . (`elem` "\\\""))
                          , "\\" ]
    pText = JText <$> pString
    pPair = (,) <$> pString <*. ":" <*> pJSON
    pObject = JObject <$> "{" .*> (pPair `sepBy` ",") <*. "}"
    pArray = JArray <$> "[" .*> (pJSON `sepBy` ",") <*. "]"
```

# Why not Haskell?

* Lots of new terminology
* Mutable state takes more effort
* Laziness changes how you need to reason about code
* Once you get used to it, these aren't problematic

# {#terminology}

<em>A monad is just a monoid in the category of endofunctors, what's
the problem?</em>

Terminology from category theory can be intimidating (at first)!

`return` probably doesn't mean what you think it means.

# {#laziness-behavior-1 .big-code}

```haskell
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs
```

# {#laziness-behavior-2 .big-code}

```haskell
sum :: Num [a] => [a] -> a
sum = go 0
  where
    go acc (x:xs) = go (acc + x) (go xs)
    go acc []     = acc
```

# {#laziness-behavior-3 .big-code}

```haskell
sum :: Num [a] => [a] -> a
sum = go 0
  where
    go acc _
      | acc `seq` False = undefined
    go acc (x:xs)       = go (acc + x) (go xs)
    go acc []           = acc
```

# {#laziness-behavior-4 .big-code}

```haskell
{-# LANGUAGE BangPatterns #-}

sum :: Num [a] => [a] -> a
sum = go 0
  where
    go !acc (x:xs) = go (acc + x) (go xs)
    go  acc []     = acc
```

# Learn More

Books
~   [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
~   [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929)
~   [Real World Haskell](http://book.realworldhaskell.org/)
Lectures
~   [Introduction to Haskell](http://www.seas.upenn.edu/~cis194/) -
    CIS 194 Spring 2013, UPenn
~   [Functional Systems in Haskell](http://www.scs.stanford.edu/14sp-cs240h/) -
    CS240h Autumn 2014, Stanford
~   [Introduction to Haskell](http://shuklan.com/haskell/index.html) -
    CS1501 Spring 2013, UVA
~   [Haskell Track](http://courses.cms.caltech.edu/cs11/material/haskell/) -
    CS 11 Fall 2011, Caltech
Practice
~   [exercism.io](http://exercism.io/),
    [Talentbuddy](http://www.talentbuddy.co/),
    [HackerRank](https://www.hackerrank.com/)
~   [H-99](http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems),
    [Project Euler](http://projecteuler.net/)

# Thanks!

+-------------+-------------------------------------------------+
| **Slides**  | [bob.ippoli.to/beginning-haskell-bayhac-2014]      |
+-------------+-------------------------------------------------+
| **Source**  | [github.com/etrepum/beginning-haskell-bayhac-2014] |
+-------------+-------------------------------------------------+
| **Email**   | bob@redivi.com                                  |
+-------------+-------------------------------------------------+
| **Twitter** | [&#64;etrepum](https://twitter.com/etrepum)     |
+-------------+-------------------------------------------------+

<!--
Other interesting presentations:
http://shuklan.com/haskell/lec01.html
http://ugcs.net/~keegan/talks/why-learn-haskell/talk.pdf

TODO
http://www.haskell.org/haskellwiki/Learn_Haskell_in_10_minutes
http://www.haskell.org/tutorial/goodies.html

-->
[bob.ippoli.to/beginning-haskell-bayhac-2014]: http://bob.ippoli.to/beginning-haskell-bayhac-2014/
[github.com/etrepum/beginning-haskell-bayhac-2014]: https://github.com/etrepum/beginning-haskell-bayhac-2014/
[hdevtools]: https://github.com/bitc/hdevtools
[ghc-mod]: http://www.mew.org/~kazu/proj/ghc-mod/en/
[HLint]: http://community.haskell.org/~ndm/hlint/
[Pandoc]: http://johnmacfarlane.net/pandoc/
