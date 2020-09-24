main :: IO ()
main = interact $ output . map (uncurry solve) . input

input :: String -> [(Integer,Integer)]
input = map (parse . words) . lines
    where
        parse [x,y] = (read x,read y)

solve :: Integer -> Integer -> Integer
solve = abs ... (-)

output :: [Integer] -> String
output = unlines . map show

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g x y = f (g x y)
