module TypeFamilies where

import Data.Text as T
import Data.List
import Data.Char
import Data.Int

-- functional dependencies

class ConsibleFD m a | m -> a where
  consFD :: a -> m -> m

instance ConsibleFD [a] a where
  consFD :: a -> [a] -> [a]
  consFD itm lst = itm : lst

instance ConsibleFD Text Char where
  consFD :: Char -> Text -> Text
  consFD = T.cons

{-
  ╬╗> :t consFD
  consFD :: ConsibleFD m a => a -> m -> m
-}

-- consFDible type families

class ConsibleTF m where
  type Item m :: *
  consTF :: Item m -> m -> m

instance ConsibleTF [a] where
  type Item [a] = a

  consTF :: a -> [a] -> [a]
  consTF itm lst = itm : lst

instance ConsibleTF Text where
  type Item Text = Char
  
  consTF :: Char -> Text -> Text
  consTF = T.cons

{-
  ╬╗> :t consTF
  consTF :: ConsibleTF m => Item m -> m -> m
-}

demoFunDepTxt :: Text
demoFunDepTxt = consFD 'a' "bcdef" 
{- hover over
  consFD :: Char -> Text -> Text
  consFD :: forall m a. ConsibleFD m a => a -> m -> m
  HaskellNotes TypeFamilies
-}

demoFunDepInt :: [Int]
demoFunDepInt = consFD 1 [2,3,4,5,6]
{- hover over
  consFD :: Int -> [Int] -> [Int]
  consFD :: forall m a. ConsibleFD m a => a -> m -> m
  HaskellNotes TypeFamilies
-}

demoTypeFamTxt :: Text
demoTypeFamTxt = consTF 'a' "bcdef" 
{- hover over
  consTF :: Item Text -> Text -> Text
  consTF :: forall m. ConsibleTF m => Item m -> m -> m
  HaskellNotes TypeFamilies
-}

demoTypeFamInt :: [Int]
demoTypeFamInt = consTF 1 [2,3,4,5,6]

{- test
╬╗> demoFunDepTxt
"abcdef"

╬╗> demoTypeFamTxt
"abcdef"

╬╗> demoFunDepInt
[1,2,3,4,5,6]

╬╗> demoTypeFamInt
[1,2,3,4,5,6]
-}

