import Data.Bifunctor (bimap,first)
import Control.Monad (join)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = interact $ output . uncurry solve . input

input :: String -> (Int,[Int])
input s = case (map read . words) s of
    (_:b:a) -> (b,a)

solve :: Int -> [Int] -> Int
solve = (.) ( uncurry possibities
            . join bimap ( frequency
                         . sumsOfPrefixes )
            . first ( map negate )
            . join bimap ( map ( pred
                               . fromEnum ) )
            . split EQ )
      . map
      . compare

split :: (Eq a) => a -> [a] -> ([a],[a])
split x = go []
    where
        go front [] = error $ "Couldn't find element in sequence"
        go front (y:ys)
            | x == y    = (front,ys)
            | otherwise = go (y:front) ys

sumsOfPrefixes :: (Num a) => [a] -> [a]
sumsOfPrefixes = go [0]
    where
        go []             _  = error "Something went wrong"
        go sums           [] = sums
        go sums@(s:_) (x:xs) = seq s $ go (x + s : sums) xs

frequency :: (Ord a) => [a] -> [(a,Int)]
frequency xs = Map.toAscList . Map.fromListWith (+) $ [(x,1 :: Int) | x <- xs]

possibities :: (Ord a,Num b) => [(a,b)] -> [(a,b)] -> b
possibities = go 0
    where
        go n [] _  = n
        go n _  [] = n
        go n xs@((x,a):xs') ys@((y,b):ys')
            | x == y = go (n + a * b) xs' ys'
            | x <  y = go n xs' ys
            | x >  y = go n xs ys'

output :: Int -> String
output = show
