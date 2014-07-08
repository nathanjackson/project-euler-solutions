-- Project Euler
-- Problem 18 (http://projecteuler.net/problem=18)
-- Nathan Jackson

import System.Environment
import System.IO

-- Read in a text file containing the problem data, run the algorithm and
-- print the result to the console.
main :: IO ()
main = do
    cmdargs <- getArgs
    withFile (head cmdargs) ReadMode (\handle -> do
        contents <- hGetContents handle
        let inputData = parse contents
        putStrLn $ show $ maxPathSum inputData)

-- Given a string whose format looks something like this:
--   1
--   2 3
--   4 5 6
--   7 8 9 0
-- Convert it to a list of lists that contains integers where each nested list
-- represents a line from the original string.
parse :: String -> [[Int]]
parse s  = map (map (read :: String -> Int)) (map words $ lines s)

-- Reduces a list of lists containing integers to its maximum path sum.
maxPathSum :: [[Int]] -> Int
maxPathSum [[x]] = x
maxPathSum xs =
    let maxPairs ys = zipWith max ys $ tail ys
        sx = reverse xs
    in maxPathSum $ ((init . init) xs) ++ [zipWith (+) (maxPairs (head sx)) $ sx !! 1]
