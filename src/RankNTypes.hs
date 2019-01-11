module RankNTypes where

import           Foundation.Extended hiding (id, uncons)
import qualified Prelude             as P
import System.Random
import Control.Monad.State

-- Source: https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

-- enables higher-rank polymorphism

-- Monomprphic function

intId :: Integer -> Integer
intId x = x

doubleId :: Double -> Double
doubleId x = x

-- parametric polymorphism aka. genrics in other languages
id' :: a -> a
id' x = x

-- Rank-1 polymorphism

-- requires extension {-# LANGUAGE RankNTypes #-}
{-
  Called the id function but represents a family of functions ~ We should really
  say that id is an identity function for all types a.
-}

-- with Rank-N-Types can annotate as follows
{-
 It is fair to say that it is an abstract function (as opposed to a concrete one), because its
 type abstracts over the type variable a. The common and proper mathematical wording is that the
 type is universally quantified (or often just quantified) over a.
-}
id :: forall a. a -> a
id a = a

-- when applied we instatiate the  type variable a
three = id (3 :: Int)

demo = P.print (id (3 :: Integer), id' "blah")

-- Rank-2 and higher polymorphism
{-
  So far we have only enabled the extension to allow us to be more explicit about the “for all” part.
  This alone is just a syntactic change and adds no new expressivity.
  However, we can use this new syntax within a type alias:
-}

type IdFunc = forall a. a -> a

{-
  Notice that the type variable is gone entirely.
  A much more interesting way to use IdFunc is as the domain of a function
-}

someInt :: IdFunc -> Integer
{-
Isn’t this curious? Since any value of type IdFunc must be the identity
function the someInt function is a function that expects the identity
function as its argument and returns an integer. Let’s give it some
(arbitrary) definition:
-}

someInt idf = idf 3

demo1 = someInt id
-- > 3

-- The someInt function isn’t even polymorphic! Rather it expects a polymorphic
-- function as its argument. This becomes clear when we expand the type alias:

someIntex :: (forall a. a -> a) -> Integer
someIntex id'' = id'' 3

{-
This function is completely monomorphic. Its type is not quantified.
When we apply a polymorphic function like id we get to choose which
types to instantiate as. The someInt function does not give us such a choice.
In fact it requires us to pass a sufficiently polymorphic function to it such
that it can make that choice. When we apply it, we need to give it choice.
If this does not make sense, look at it using the promise/demand interpretation.
The identity function makes a promise. It promises to work for all a.
When you apply it, you demand a to be a certain type. However, the someInt
function makes no such promise. It wants us to pass it a function that
makes a promise, such that it gets to demand something from it.
We don’t get to demand anything.
This is called rank-2 polymorphism. You can have arbitrary-rank
polymorphism by burying the quantifier in more levels of necessary
parentheses. Example:
-}

-- type IdFunc = forall a. a -> a

type SomeInt = IdFunc -> Integer

someOtherInt :: SomeInt -> Integer
someOtherInt someInt' =
    someInt' id + someInt' id


-- Example: random numbers

data Player =
    Player {
      playerName :: String,
      playerPos  :: (Double, Double)
    }
    deriving (Eq, Ord, Show)


-- generate large recursive data structure

-- randomPlayer :: (RandomGen g) => g -> (Player, g)

{-
But we want to do more. Since the data structure is so huge, we want
to print some progress information while we’re generating it.

This requires IO of course. Rather than enforcing a certain transformer
stack we would just request a sufficiently featureful monad by using
effect classes:
-}

-- import Control.Monad.State

-- randomPlayer
--     :: (MonadIO m, MonadState g m, RandomGen g)
--     => m Player

{-
However, the user of randomPlayer may already be using a state monad for something else,
or the random number generator may actually live in a mutable variable. You might even
run into a case where the random number generator is completely hidden global state,
so all you get is monadic actions. At this point things start to become really awkward.

But with higher-rank polymorphism there is actually a very simple solution.
The first step is not to request an explicit functional representation of the
random-number generator, but rather just some monad m that provides some means
of random number generation. Then generating a random number (or really anything
else with a Random instance) is a matter of performing a certain m-action.

We can write type aliases for these m-actions:

-}

type GenAction m = forall a. (Random a) => m a

type GenActionR m = forall a. (Random a) => (a, a) -> m a

{-
A value of type GenAction m is an m-action that supposedly produces
a random element of whatever type we request, as long as there is a
Random instance. The GenActionR type represents the ranged variants.

One simple example is the action that generates a random number in a
state monad, when the state is a generator:
-}

genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

{-
  If we expand the GenAction alias and simplify (we will learn how to do that later),
  then the type of genRandom becomes:
-}

genRandom' :: (Random a, RandomGen g) => State g a
genRandom' = undefined

{-
  Now we can write a function that requests such a random number
  generator as its argument:
-}

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
    liftIO (putStrLn "Generating random player...")

    len <- genR (8, 12)
    name <- replicateM len (genR ('a', 'z'))
    x <- genR (-100, 100)
    y <- genR (-100, 100)

    liftIO (putStrLn "Done.")
    pure (Player (toStr name) (x, y))

{-
  Notice how the function uses the fact that it receives a polymorphic
  function as its argument. It instantiates its type variable as various
  different types, including Int (for len) and Char (for name).

  If you have some global-state random number generator, then this function
  is actually surprisingly easy to use. The randomRIO function from System.

  Random is such a function:
-}

-- randomRIO :: (Random a) => (a, a) -> IO a

-- This type signature fits the GenActionR type,

-- so we can pass it to randomPlayer:

main :: IO ()
main = randomPlayer randomRIO >>= P.print


--- Scott encoding ~ custom version of list

-- construct list
data List a
    = Cons a (List a)
    | Nil

-- deconstruct
uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co ni (Cons x xs) = co x xs
uncons co ni Nil         = ni

{-
  This function takes two continuations and a list. The continuations determine
  what we reduce the list into depending on which constructor is found.

  Here is a simple example:
-}

listIsNull :: List a -> Bool
listIsNull = uncons (\_ _ -> False) True

{-
  So we have a way to construct lists by using the List constructors,
  and we have a way to deconstruct lists by unconsing, by using the
  pattern-matching combinator uncons. However, this is actually an
  indirection. Interestingly a list is actually fully determined by
  what happens when you uncons it. That means we can represent a list
  in terms of its uncons operator, which is called Scott encoding and
  requires a rank-2 type:
-}

newtype ListS a =
    ListS {
      unconsS :: forall r. (a -> ListS a -> r) -> r -> r
    }

-- You may have noticed that the list argument is missing from unconsS,
-- but actually it is not. It is implicit, because it is an accessor function

-- unconsS :: ListS a -> (forall r. (a -> ListS a -> r) -> r -> r)
-- which is equivalent to:
-- unconsS :: ListS a -> (a -> ListS a -> r) -> r -> r
