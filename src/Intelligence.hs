module Intelligence where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Random (StdGen, randomR, randomRs)

-- | Algorithm gratefully borrowed from
-- https://golang.org/doc/codewalk/markov/
-- And:
-- https://blog.codinghorror.com/markov-and-you/

type DB = M.Map (V.Vector T.Text) (V.Vector T.Text)

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

-- | Groups the values of a list into a Map, with unique keys.
-- TODO: Make this more general type-wise.
groupToMap :: [(V.Vector T.Text, T.Text)] -> DB
groupToMap xs = M.fromListWith (V.++) $ map (fmap V.singleton) xs

-- | Create a database of Markov wisdom from a prefix length and a list of texts.
createDatabase :: Int -> [T.Text] -> DB
createDatabase prefixLen texts = groupToMap $ concatMap (toMarkovPairs prefixLen) texts

-- | Create the first phrase of a message.
firstPhrase :: StdGen -> DB -> (V.Vector T.Text, StdGen)
firstPhrase g db = let (pidx,   g' ) = randomR1 g  $ (M.size   db)-1
                       (sidx,   g'') = randomR1 g' $ (V.length ss)-1
                       (prefix, ss ) = M.elemAt pidx db
                    in (V.snoc prefix (ss V.! sidx), g'')

-- | Generate a randomly selected suffix for a prefix, and append it to the suffix.
-- The first argument is the length of the prefix in the database.
nextPhrase :: Int -> V.Vector T.Text -> StdGen -> DB -> Maybe (V.Vector T.Text, StdGen)
nextPhrase len prefix g db = let p' = V.slice (((V.length prefix)-1) - len) len prefix
                              in case M.lookup p' db of
                                   Nothing -> Nothing
                                   Just ws -> let (widx, g') = (randomR1 g $ (V.length ws)-1)
                                               in Just (V.snoc prefix (ws V.! widx), g')

-- | Use the Markov-chain to generate a line of a certain length.
generateLine :: Int -> Int -> StdGen -> DB -> (T.Text, StdGen)
generateLine prefixes words g db = (V.foldl1 (\t1 t2 -> t1 `T.append` (' ' `T.cons` t2)) result, g'')
  where
    (first, g')   = firstPhrase g db
    (result, g'') = generateLine' prefixes words first g' db

    generateLine' :: Int -> Int -> V.Vector T.Text -> StdGen -> DB -> (V.Vector T.Text, StdGen)
    generateLine' p w ts g db = case nextPhrase p ts g db of
                                  Nothing        -> (ts, g)
                                  Just (ts', g') -> if V.length ts' >= w
                                                       then (ts', g')
                                                       else let (ts'', g'') = generateLine' p w ts' g' db
                                                             in (ts' V.++ ts'', g'')
