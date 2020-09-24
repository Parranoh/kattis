import Text.Printf

main :: IO ()
main = interact $ output . map (uncurry $ flip solve 1) . input

input :: String -> [(Double,Int)]
input = map parse . map words . init . lines
    where
        parse [n,t] = (read t,read n)
        parse _     = error "Weird line"

        -- t       money      n
solve :: Double -> Double -> Int -> Double
solve t = go
    where
        go x 0 = x -- no questions left, so quit
        go x n =
            let x' = go (2 * x) (n - 1) -- expected prize after answering the next question correctly
                equil = max t $ x / x' -- minimal success probability to attempt next question
                pTry = (1 - equil) / (1 - t) -- probability of attempt
                pSucc = (1 + equil) / 2 -- probability of success in case of attempt
            in (1 - pTry) * x + pTry * pSucc * x'

output :: [Double] -> String
output = unlines . map (printf "%.3f")
