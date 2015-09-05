module Intelligence (createDatabase, generateLine) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Random (StdGen, randomR, randomRs)

-- | Algorithm gratefully borrowed from
-- https://golang.org/doc/codewalk/markov/
-- And:
-- https://blog.codinghorror.com/markov-and-you/

-- | Maps prefixes to the a list of the next word.
-- Also contains the length of the prefix.
data DB = DB {
  database     :: M.Map (V.Vector T.Text) (V.Vector T.Text),
  prefixLength :: Int
}

-- | A specialisation of the randomR-function.
randomR1 :: StdGen -> Int -> (Int, StdGen)
randomR1 g max = randomR (0, max) g

-- | The order of the Markov chain, or length of the prefix, in number of words.
-- Returns a list of prefixes and their suffixes.
toMarkovPairs :: Int -> T.Text -> [(V.Vector T.Text, T.Text)]
toMarkovPairs i s = zipWith combine prefixes (tail prefixes)
  where
    words       = V.fromList $ filter (not . T.null) $ T.splitOn " " s
    count       = V.length words
    prefixes    = map (\start -> V.slice start i words) [0..count-i]
    combine a b = (a, V.last b)

-- | Create a database of Markov wisdom from a prefix length and a list of texts.
createDatabase :: Int -> [T.Text] -> DB
createDatabase prefixLen texts = DB { database = M.fromListWith (V.++) 
                                               $ map (fmap V.singleton) 
                                               $ concatMap (toMarkovPairs prefixLen) texts,
                                      prefixLength = prefixLen
                                    }


-- | Create the first phrase of a message.
firstPhrase :: StdGen -> DB -> (V.Vector T.Text, StdGen)
firstPhrase g db = let (pidx,   g' ) = randomR1 g  $ (M.size   $ database db)-1
                       (sidx,   g'') = randomR1 g' $ (V.length            ss)-1
                       (prefix, ss ) = M.elemAt pidx $ database db
                    in (V.snoc prefix (ss V.! sidx), g'')

-- | Generate a randomly selected suffix for a prefix, and append it to the suffix.
nextPhrase :: V.Vector T.Text -> StdGen -> DB -> Maybe (V.Vector T.Text, StdGen)
nextPhrase prefix g db = let len = prefixLength db
                             p'  = V.slice (((V.length prefix)-1) - len) len prefix
                          in case M.lookup p' (database db) of
                               Nothing -> Nothing
                               Just ws -> let (widx, g') = (randomR1 g $ (V.length ws)-1)
                                           in Just (V.snoc prefix (ws V.! widx), g')

-- | Use the Markov-chain to generate a line of a certain length.
generateLine :: Int -> StdGen -> DB -> (T.Text, StdGen)
generateLine length g db = (V.foldl1 (\t1 t2 -> t1 `T.append` (' ' `T.cons` t2)) result, g'')
  where
    (first, g')   = firstPhrase g db
    (result, g'') = generateLine' length first g' db

    generateLine' :: Int -> V.Vector T.Text -> StdGen -> DB -> (V.Vector T.Text, StdGen)
    generateLine' w ts g db = case nextPhrase ts g db of
                                  Nothing        -> (ts, g)
                                  Just (ts', g') -> if V.length ts' >= w
                                                       then (ts', g')
                                                       else let (ts'', g'') = generateLine' w ts' g' db
                                                             in (ts' V.++ ts'', g'')
