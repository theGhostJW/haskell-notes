module FunctionInfixFunctions where

import           Foundation.Extended
import qualified Prelude             as P
import Basement.String as S
import qualified Data.Text as T

-- Precedence Levels 0 - 9 where 9 is the stickyest

-- White Space ( ) :: forall r a (b :: TYPE r). (a -> b) -> a -> b infixl 10
-- http://zvon.org/other/haskell/Outputsyntax/fixityQdeclaration_reference.html 
-- * normal application is assumed to have a precedence level of 10

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

fullName :: String -> String -> String -> String
fullName fstn othn surn = fstn <> " " <> othn <> " " <> surn

{-
╬╗> fullName "Robert" "Louis" "Stevenson"
"Robert Louis Stevenson"
-}

fullRobert :: String -> String -> String
fullRobert = fullName "Robert"

{-
╬╗> fullRobert "Roy" "MacGregor"
"Robert Roy MacGregor"
-}

fullRobertL :: String -> String
fullRobertL = fullName "Robert" "Louis Balfour"

{-
╬╗> fullRobertL "Stevenson"
"Robert Louis Balfour Stevenson"
-}

addEm :: Int -> Int -> Int
addEm i1 i2 = i1 + i2

(++++) :: Int -> Int -> Int
i1 ++++ i2 = i1 + i2 
{-
╬╗> addEm 2 3
5

╬╗> 2 ++++ 3
5

╬╗> 2 `addEm` 3
5

╬╗> (++++) 2 3
5

-}

s :: Integer
s = 1 - 2 - 3 - 4
-- parsed as : 
{-
╬╗>  s
-8
-}

p :: Integer
p = 2 ^ (3 :: Word64) ^ (4 :: Word64)
{-
╬╗> p
2417851639229258349412352
-}

--  b :: Bool
 -- b = True == True == False == False
{-
Precedence parsing error
    cannot mix `==' [infix 4] and `==' [infix 4] in the same infix expression(undefined)
-}

b1 :: Bool
b1 = (True == True) == (False == False)
{-
╬╗> b1
True
-}


-- parsed as : 
{-
╬╗>  s
-8
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
                 
                    fullNme :: String -> String -> String
                    fullNme gvn sur = (toS . T.strip $ toS gvn) <> sur
                  in
                    case S.splitOn (',' ==) schlRoleName of 
                      [_, g] -> fullNme g h
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

Associativity: 
https://riptutorial.com/haskell/example/16493/associativity

when the same infixes are next to each other
infixl ~ start bracketing from the left
infixr ~ start bracketing from the right
infix  ~ no defualt brackets - must include brackets

Operators without an explicit fixity declaration are infixl 9

-}

{-
http://zvon.org/other/haskell/Outputsyntax/fixityQdeclaration_reference.html 
* normal application is assumed to have a precedence level of 10

( ) :: forall r a (b :: TYPE r).(a -> b) -> a -> b infixl 10

($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b infixr 0
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




