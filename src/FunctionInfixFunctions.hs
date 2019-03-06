module FunctionInfixFunctions where

import           Foundation.Extended
import qualified Prelude             as P
import Basement.String as S
import qualified Data.Text as T

-- Precedence Levels 0 - 9 where 9 is the stickyest

-- Associativity {l , r, n} https://www.reddit.com/r/haskell/comments/5q9719/function_application_is_left_associative/dcxclgt
{- e.g. function composition is left associative
it means that if you have 2 functions:

  f x = x + 1
  g x = x + 2
  
  and you have the expression
  
  f g 5
  
  This expression will be parsed
  
  (f g) 5
  
  rather than, if it were right associative:
  
  f (g 5)
  
  It associates to the left.
-}

-- White Space ( ) :: forall r a (b :: TYPE r). (a -> b) -> a -> b infixl 9
-- https://stackoverflow.com/a/3125414/5589037

-- Composition https://www.stackage.org/haddock/lts-13.10/base-4.12.0.0/Prelude.html#v:const
-- (.) :: (b -> c) -> (a -> b) -> a -> c infixr 9

-- $low Apply  https://www.stackage.org/haddock/lts-13.10/base-4.12.0.0/Prelude.html#v:-36-
-- ($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b infixr 0
{-
  Application operator. This operator is redundant, since ordinary application (f x) means the same as (f $ x). 
  However, $ has low, right-associative binding precedence, so it sometimes allows parentheses to be omitted; for example:

  f $ g $ h x  =  f (g (h x))

  It is also useful in higher-order situations, such as map ($ 0) xs, or zipWith ($) fs xs.

-}

allCaps :: String -> String
allCaps = upper

_allCaps = allCaps "Kylie"
-- >>> allCaps "Kylie"
-- "KYLIE"

shoutIt :: Bool -> String -> String
shoutIt loud wrds = wrds <> bool " !" " !!!!!!!!" loud <> " <-- i'm shouting"

_shoutItTrue = shoutIt True "Kylie"
-- >>> shoutIt True "Kylie"
-- "Kylie !!!!!!!! <-- i'm shouting"

_shoutItFalse = shoutIt False "Kylie"
-- >>> shoutIt False "Kylie"
-- "Kylie ! <-- i'm shouting"


{-# ANN shoutItOutLoud ("HLint: ignore Eta reduce" :: P.String) #-}
shoutItOutLoud :: String -> String 
shoutItOutLoud str = shoutIt True str

_shoutItOutLoud = shoutItOutLoud "Kylie"
-- >>> shoutItOutLoud "Kylie"
-- "Kylie !!!!!!!! <-- i'm shouting"

schoolRole :: String -> String -> String
schoolRole givenName surname = surname <> ", " <> givenName

_schoolRole = schoolRole "Kylie" "Minogue"
-- >>> schoolRole "Kylie" "Minogue"
-- "Minogue, Kylie"

-- partial application / point free
oilsRole :: String -> String
oilsRole = schoolRole "Peter"

_oilsRole = oilsRole "Minogue"
-- >>> oilsRole "Minogue"
-- "Minogue, Peter"

guitars :: String -> String
guitars schlRoleName = 
                  let 
                    h :: String
                    h = " Hendrix"
                 
                    fullName :: String -> String -> String
                    fullName gvn sur = (toS . T.strip $ toS gvn) <> sur
                  in
                    case S.splitOn (',' ==) schlRoleName of 
                      [_, g] -> fullName g h
                      _ -> "Unexpected Name Format"

_guitars = guitars "Minogue, Kylie"
-- >>> guitars "Minogue, Kylie"
-- "Kylie Hendrix"

-- shout it out at school
-- _shoutItOutAtSchool = shoutItOutLoud schoolRole "Kylie" "Minogue"
-- * Error - The function `shoutItOutLoud' is applied to three arguments, but its type `String -> String' has only one

-- _shoutItOutAtSchool = shoutItOutLoud schoolRole $ "Kylie" "Minogue"
-- * Couldn't match expected type `t0 -> t' with actual type `String'
-- same as 
-- _shoutItOutAtSchool = shoutItOutLoud schoolRole ("Kylie" "Minogue") <--Nuts

_shoutItOutAtSchool = shoutItOutLoud $ schoolRole "Kylie" "Minogue"
-- "Minogue, Kylie !!!!!!!! <-- i'm shouting"

{-

( ) :: (b -> c) -> (a -> b) -> a -> c infixr 9
(.) :: (b -> c) -> (a -> b) -> a -> c infixr 9
_shoutItOutAtSchool = shoutItOutLoud . schoolRole "Kylie" "Minogue"
===
_shoutItOutAtSchoolc = shoutItOutLoud . (schoolRole "Kylie" "Minogue") <- Nuts

* Couldn't match expected type `a -> String'
              with actual type `String'
* Possible cause: `schoolRole' is applied to too many arguments
  In the second argument of `(.)', namely
    `schoolRole "Kylie" "Minogue"'
  In the expression: shoutItOutLoud . schoolRole "Kylie" "Minogue"
-}

_shoutItOutAtSchoolc = shoutItOutLoud . schoolRole "Kylie" $ "Minogue"
-- "Minogue, Kylie !!!!!!!! <-- i'm shouting"

_goBeserk2 = upper $ shoutItOutLoud $ guitars $ oilsRole "Minogue"
-- "PETER HENDRIX !!!!!!!! <-- I'M SHOUTING"
-- _goBeserk2 = upper (shoutItOutLoud (guitars (oilsRole "Minogue")))

_goBeserk3 = upper $ shoutItOutLoud . guitars $ oilsRole "Minogue"
-- "PETER HENDRIX !!!!!!!! <-- I'M SHOUTING"
-- _goBeserk3 = upper $ ((shoutItOutLoud . guitars) (oilsRole "Minogue"))
-- _goBeserk3 = upper $ (shoutItOutLoud . guitars (oilsRole "Minogue"))

_goBeserk = upper . shoutItOutLoud . guitars $ oilsRole "Minogue"
-- "PETER HENDRIX !!!!!!!! <-- I'M SHOUTING"
-- _goBeserk = (upper . shoutItOutLoud . guitars) (oilsRole "Minogue")
-- _goBeserk = upper . shoutItOutLoud . guitars (oilsRole "Minogue")

-- _goBeserkerr = upper . shoutItOutLoud . guitars . oilsRole "Minogue"
-- ERR * Couldn't match expected type `a -> String' with actual type `String'

_goBeserk0 = upper . shoutItOutLoud . guitars . oilsRole $ "Minogue"
-- "PETER HENDRIX !!!!!!!! <-- I'M SHOUTING"
-- _goBeserk00 = (upper . shoutItOutLoud . guitars . oilsRole) "Minogue"




