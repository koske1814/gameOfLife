import System.Random
import Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine
  gen0 <- newStdGen
  let ns = randomRs (0,1) gen0 
  let xs = makeBoard n n (take (n*n) ns :: [Int])
  forM_ xs $ \x ->
    print $ x
  
makeBoard :: Int -> Int -> [Int] -> [[Int]]
makeBoard 1 n xs = [xs]
makeBoard z n xs = (take n xs) : makeBoard (z-1) n (drop n xs)
