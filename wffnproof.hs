import Data.List (transpose)

main :: IO ()
main = interact $ output . map solve . input

input :: String -> [[Char]]
input = init . lines

solve :: [Char] -> [Char]
solve letters = case part letters of
    ([],_,_) -> "no WFF possible"
    (vs,os,ns) -> ns ++ (concat . transpose) [os,vs]

part :: [Char] -> ([Char],[Char],[Char])
part ls = (take (len + 1) vars,take len ops,nots)
    where
        len = min (length vars - 1) (length ops)
        (vars,ops,nots) = go ([],[],[]) ls
        go r@(_,_,_) [] = r
        go (v,o,n) (c:cs)
            | c `elem` "pqrst" = go (c:v,o,n) cs
            | c `elem` "KACE"  = go (v,c:o,n) cs
            | c == 'N'         = go (v,o,c:n) cs

output :: [[Char]] -> String
output = unlines
