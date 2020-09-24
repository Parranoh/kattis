main :: IO ()
main = interact (output . solve . input)

                 -- row col
input :: String -> (Int,Int,[[Char]])
input = parse . lines
    where
        parse (l:ls) = (read $ words l !! 2,read $ words l !! 3,ls)

solve :: (Int,Int,[[Char]]) -> [[Char]]
solve (row,col,ls) = concatMap (replicate row) . map (concatMap $ replicate col) $ ls

output :: [[Char]] -> String
output = unlines
