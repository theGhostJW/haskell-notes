module FunctionInfixFunctions where

import           Foundation.Extended
import qualified Prelude             as P



($$$) :: (a -> b) -> a -> b
($$$) fn arg = fn arg

-- Eta reduce
($$$%) :: (a -> b) -> a -> b
($$$%) fn = fn

-- Difference is precedence
