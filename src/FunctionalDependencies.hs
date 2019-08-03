module FunctionalDependencies where

import Prelude as P
import Control.Monad.State


newtype Supply stateItem answer = Supply (State [stateItem] answer)
                      deriving (P.Functor, P.Applicative, P.Monad)

class (Monad monad) => MonadSupply val monad | monad -> val where
  nxt :: monad (Maybe val)

instance MonadSupply a (Supply a) where
  nxt :: Supply a (Maybe a)
  nxt = undefined

-- OK because (Supply String) is different to (Supply a)
instance MonadSupply String (Supply String) where
  nxt :: Supply String (Maybe String)
  nxt = undefined

-- OK because (Supply String) is different to (Supply a)
instance MonadSupply Char (Supply Char) where
  nxt :: Supply Char (Maybe Char)
  nxt = undefined

--- https://stackoverflow.com/questions/20526828/motivation-of-having-functional-dependencies
--- https://stackoverflow.com/questions/12956063/functional-dependencies-vs-type-families


{-
-- OK because (Supply String) is different to (Supply a)
instance MonadSupply Int (Supply String) where
  nxt :: Supply Int (Maybe String)
  nxt = undefined

Functional dependencies conflict between instance declarations:
  instance MonadSupply a (Supply a)
    -- Defined at C:\Users\thegh\AppData\Local\Temp\ghc-mod4845\FunA7BB.hs:14:10
  instance MonadSupply Int (Supply String)
    -- Defined at C:\Users\thegh\AppData\Local\Temp\ghc-mod4845\FunA7BB.hs:25:10(undefined)

-}

{-
newtype Supply stateItem answer = Supply (State [stateItem] answer)
                      deriving (Functor, Applicative, Monad)

class (Monad monad) => MonadSupply val monad | monad -> val where
    nxt :: monad (Maybe val)

instance MonadSupply a (Supply a) where
  nxt :: Supply a (Maybe a)
  nxt = undefined

-- OK because (Supply String) is different to (Supply a)
instance MonadSupply String (Supply String) where
  nxt :: Supply String (Maybe String)
  nxt = undefined

data Example1 = Example1 deriving (Show)

class KnowsA a b | b -> a where
    known :: b -> a

-- not OK Example1 : -> Int
-- and Example 1   : -> String
-- instance KnowsA Int Example1 where
--      known = const 1

instance KnowsA String Example1 where
   known = const "blahh"

-- Segway - functional dependency

https://stackoverflow.com/a/20040427/5589037

data Example1 = Example1 deriving (Show)

class KnowsA a b | b -> a where
    known :: b -> a

instance KnowsA Int Example1 where
    known = const 1

instance KnowsA String Example1 where
   known = const "blahh"

   * Illegal instance declaration for `KnowsA String Example1'
       (Only one type can be given in an instance head.
        Use MultiParamTypeClasses if you want to allow more, or zero.)
   * In the instance declaration for `KnowsA String Example1'

  this does not work either - is functional dependency the default
  class KnowsA a b  where
    known :: b -> a

  data Example1 = Example1 deriving (Show)

  class KnowsA a b where
      known :: b -> a

  instance KnowsA Bool Example1 where
      known = "Hi"

  So without Multiparameter type classes you must:
    1. declare functional dependency and only declare one instance
    2. only use one type in instance head

-}