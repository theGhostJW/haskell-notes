{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module PolysemyDemo where 

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Prelude
import Polysemy.Error
import Polysemy.Resource

-- Teletype effect:

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a -- Lift to Embed
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg


runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure  -- For each WriteTTY in our program, consume an output by appending it to the list in a ([String], a)
  . runInputList i         -- Treat each element of our list of strings as a line of input
  . reinterpret2 
      \case     -- Reinterpret our effect in terms of Input and Output
        ReadTTY -> maybe "" id <$> input
        WriteTTY msg -> output msg


echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i *> echo

-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

-- echo forever
echoForever :: IO ()
echoForever = runM . teletypeToIO $ echo

-- Resource effect:

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work $ \e -> writeTTY ("Caught " ++ show e)
  where work = bracket (readTTY) (const $ writeTTY "exiting bracket") $ \txtIn -> do
          writeTTY "entering bracket"
          case txtIn of
            "explode"     -> throw ThisException
            "weird stuff" -> writeTTY txtIn >> throw ThatException
            _             -> writeTTY txtIn >> writeTTY "no exceptions"

resourceEffect :: IO (Either CustomException ())
resourceEffect = (runM .@ lowerResource .@@ lowerError @CustomException) . teletypeToIO $ program

-- guard play nothing to do with
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where 
  score = strokes-par

holeScorel :: Int -> Int -> String
holeScorel strokes par =
  let 
    score = strokes-par
    rslt
      | score < 0 = show (abs score) ++ " under par"
      | score == 0 = "level par"
      | otherwise = show(score) ++ " over par"
  in 
    rslt


  
