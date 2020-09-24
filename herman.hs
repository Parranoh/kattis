main :: IO ()
main = interact $ output . solve . input

input :: String -> Double
input = read

solve :: Double -> [Double]
solve r = [pi * r * r,2 * r * r]

output :: [Double] -> String
output = unlines . map show
