import System.IO

main :: IO ()
main = go 1 1000
    where
        go a z = do
            let m = (a + z) `div` 2
            print m
            hFlush stdout
            res <- getLine
            case res of
                "correct" -> return ()
                "lower" -> go a (m - 1)
                "higher" -> go (m + 1) z
