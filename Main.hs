{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.Directory
import Data.Monoid (mconcat)
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import qualified Data.Map as Map


dictLineToEntryPair :: String -> (String, String)
dictLineToEntryPair line = (head ws, arphabet)
    where ws = words line
          arphabet = filter isAlpha (intercalate "" (tail ws))

joinNonEmpty words = unwords $ filter (not.null) words

translateWord dict word = Map.findWithDefault "" (map toUpper word) dict


main = do
    filep <- getCurrentDirectory
    print filep
    dictSource <- readFile "data/cmudict.txt"
    let dictLines = lines dictSource
    let dict =  Map.fromList (map dictLineToEntryPair dictLines)

    scotty 3000 $ do
      get "/phonemes/:sentence" $ do
          sentence <- param "sentence"
          --putStrLn ( "Searching for: " ++ sentence)
          let arphabetWords = map (translateWord dict) (words sentence)
          --print (joinNonEmpty arphabetWords)
          --(joinNonEmpty arphabetWords)
          text  (T.pack (joinNonEmpty arphabetWords))


