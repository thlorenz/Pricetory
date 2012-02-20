-- | None of the functions currently in here are used
-- They just show diffent options of implementing randomRs which is already part
-- of the System.Random package

import Random (randomR, Random, RandomGen, StdGen)
import Data.List (unfoldr)

-- | Implementation using unfoldr
-- unfoldr :: (b -> Maybe (a, b))  -> b         -> [a] 
--            (unfolding function) -> seedValue -> randoms
-- unfolds a list from the seed value b
-- returning Nothing would stop unfolding the list
-- returning Just (a, b) adds a to the list and uses b as the new seed
-- to produce infinite list always return Just (a, b)
randomsUnfoldr :: (RandomGen g, Random a) => a -> a -> g -> [a]
randomsUnfoldr min max = unfoldr (Just . randomR (min, max))

-- | Implementation using recursion 
-- unfoldr not really needed since we never stop generation by returning Nothing
-- produces randoms lazily
randomsRec :: (RandomGen g, Random a) => a -> a -> g -> [a]
randomsRec min max g = x : randomsRec min max g'
    where (x, g') = randomR (min, max) g

-- | System.Random implementation (randomRs) is almost identical to above
randomRs :: (Random a, RandomGen g) => (a,a) -> g -> [a]
randomRs ival g = x : randomRs ival g' where (x,g') = randomR ival g

-- | My initial (pretty bad) implementation (ab)using scanl
randomsScanl :: (Random a, Num a) => StdGen -> a -> a -> [b] -> [a]
randomsScanl gen min max = map fst . tail . scanl f (0, gen)
    where f (x, g) _ =  randomR (min, max) g
