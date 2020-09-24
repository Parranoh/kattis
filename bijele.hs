import qualified Control.Applicative as A

main :: IO ()
main = interact (output . solve . input)

input :: String -> A.ZipList Integer
input = A.ZipList . map read . words

solve :: A.ZipList Integer -> A.ZipList Integer
solve = ((-) <$> A.ZipList [1,1,2,2,2,8] <*>)

output :: A.ZipList Integer -> String
output = unwords . map show . A.getZipList
