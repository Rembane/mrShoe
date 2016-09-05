{-# LANGUAGE OverloadedStrings #-}
module Intelligence (DB, createDatabase, generateLine) where

import Control.Applicative ((<$>))
import Control.Monad ((=<<), mapM)
import Control.Monad.Random (Rand, RandomGen, evalRandIO, getRandomR)
import Data.Maybe (maybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Vector (Vector)
import System.Random (RandomGen, StdGen, randomR, randomRs)

-- | Algorithm gratefully borrowed from
-- https://golang.org/doc/codewalk/markov/
-- And:
-- https://blog.codinghorror.com/markov-and-you/

-- | Maps prefixes to the a list of the next word.
-- Also contains the length of the prefix.
data DB = DB
  { database     :: M.Map (Vector Text) (Vector Text)
  , prefixLength :: Int
  }

-- | The order of the Markov chain, or length of the prefix, in number of words.
-- Returns a list of prefixes and their suffixes.
toMarkovPairs :: Int -> Text -> [(Vector Text, Text)]
toMarkovPairs i s = zipWith combine prefixes (tail prefixes)
  where
    words       = V.fromList $ filter (not . T.null) $ T.splitOn " " s
    count       = V.length words
    prefixes    = map (\start -> V.slice start i words) [0..count-i]
    combine a b = (a, V.last b)

-- | Create a database of Markov wisdom from a prefix length and a list of texts.
createDatabase :: Int -> [Text] -> DB
createDatabase prefixLen texts = DB { database = M.fromListWith (V.++)
                                               $ map (fmap V.singleton)
                                               $ concatMap (toMarkovPairs prefixLen) texts,
                                      prefixLength = prefixLen
                                    }

-- | Get a random suffix from a list of suffixes.
getRandomSuffix :: RandomGen g => Vector Text -> Rand g Text
getRandomSuffix ts = (ts V.!) <$> getRandomR (0, (V.length ts)-1)

-- | Create the first phrase of a message.
firstPhrase :: RandomGen g => DB -> Rand g (Vector Text)
firstPhrase db = do
  let ddb = database db
  (prefix, ss) <- (`M.elemAt` ddb) <$> getRandomR (0, (M.size ddb)-1)
  (prefix `V.snoc`) <$> (getRandomSuffix ss)

-- | Generate a randomly selected suffix for a prefix, and append it to the suffix.
nextPhrase :: RandomGen g => Vector Text -> DB -> Rand g (Maybe (Vector Text))
nextPhrase prefix db = do
  let len = prefixLength db
  let p'  = V.slice ((V.length prefix) - len) len prefix
  case M.lookup p' $ database db of
    Nothing -> return Nothing
    Just ss -> Just <$> (V.snoc prefix) <$> getRandomSuffix ss


-- | Use the Markov-chain to generate a line consisting of up to maxLength number of chains.
-- It might be shorter if there is no good path in the database.
generateLine :: RandomGen g => Int -> DB -> Rand g Text
generateLine length db = V.foldl1 (\t1 t2 -> t1 `T.append` (' ' `T.cons` t2))
                      <$> (firstPhrase db >>= (generateLine' length db))

  where
    generateLine' :: RandomGen g => Int -> DB -> Vector Text -> Rand g (Vector Text)
    generateLine' 0         _  ts = return ts
    generateLine' maxLength db ts = maybe (return ts) (generateLine' (maxLength - 1) db) =<< (nextPhrase ts db)
