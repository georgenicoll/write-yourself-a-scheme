module Main where
import System.Environment
import System.IO

main :: IO()
main = do
    progname <- getProgName
    args <- getArgs
    name <- promptLine "What is your name?"
    putStrLn (progname ++ ": Total = " ++ show(addArgs args) ++ ", " ++ name)

addArgs :: [String] -> Integer
addArgs = foldl (\a b -> a + read b) 0

promptLine :: String -> IO String
promptLine prompt = do
    putStr $ prompt ++ ": "
    hFlush stdout
    getLine
