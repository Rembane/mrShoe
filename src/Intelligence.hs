module Intelligence (createDatabase, generateLine) where

import Control.Applicative ((<$>))
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

-- | Create the first phrase of a message.
-- TODO: Make pretty!
firstPhrase :: DB -> State StdGen (Vector Text)
firstPhrase db = do
  (prefix, ss) <- fmap (\pidx -> M.elemAt pidx $ database db) (randomR1 $ (M.size $ database db)-1)
  sidx         <- randomR1 $ (V.length ss)-1

  return $ V.snoc prefix (ss V.! sidx)

-- | Generate a randomly selected suffix for a prefix, and append it to the suffix.
nextPhrase :: Vector Text -> DB -> State StdGen (Maybe (Vector Text))
nextPhrase prefix db =
  case M.lookup p' (database db) of
      Nothing -> return Nothing
      Just ws -> do
        widx <- randomR1 $ (V.length ws)-1
        return $ Just $ V.snoc prefix $ ws V.! widx
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
