module Intelligence (createDatabase, generateLine) where

import Control.Applicative ((<$>))
import Control.Monad (mapM)
import Control.Monad.Trans.State (State, state)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Vector (Vector)
import System.Random (StdGen, randomR, randomRs)

-- | Algorithm gratefully borrowed from
-- https://golang.org/doc/codewalk/markov/
-- And:
-- https://blog.codinghorror.com/markov-and-you/

-- | Maps prefixes to the a list of the next word.
-- Also contains the length of the prefix.
data DB = DB {
  database     :: M.Map (Vector Text) (Vector Text),
  prefixLength :: Int
}

-- | A specialisation of the randomR-function which puts it in the State-monad.
-- And sets the return type to Int with 0 as the smallest return value.
randomR1 ::  Int -> State StdGen Int
randomR1 max = state $ randomR (0, max)

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
getRandomSuffix :: Vector Text -> State StdGen Text
getRandomSuffix ts = (ts V.!) <$> (randomR1 $ (V.length ts)-1)

-- | Create the first phrase of a message.
firstPhrase :: DB -> State StdGen (Vector Text)
firstPhrase db = do
  let ddb = database db
  (prefix, ss) <- (`M.elemAt` ddb) <$> (randomR1 $ (M.size ddb)-1)
  (prefix `V.snoc`) <$> (getRandomSuffix ss)

-- | Generate a randomly selected suffix for a prefix, and append it to the suffix.
nextPhrase :: Vector Text -> DB -> State StdGen (Maybe (Vector Text))
nextPhrase prefix db = (fmap (prefix `V.snoc`)) <$> (sequenceA $ getRandomSuffix <$> (M.lookup p' (database db)))

  where
    len = prefixLength db
    p'  = V.slice (((V.length prefix)-1) - len) len prefix

-- | Use the Markov-chain to generate a line of a certain length.
generateLine :: Int -> DB -> State StdGen Text
generateLine length db = V.foldl1 (\t1 t2 -> t1 `T.append` (' ' `T.cons` t2)) 
                      <$> (firstPhrase db >>= (generateLine' length db))

  where
    generateLine' :: Int -> DB -> Vector Text -> State StdGen (Vector Text)
    generateLine' w db ts = do
      np <- nextPhrase ts db 
      case np of
        Nothing  -> return ts
        Just ts' -> if V.length ts' >= w
                       then return ts'
                       else (ts' V.++) <$> generateLine' w db ts' 
