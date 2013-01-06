module Main where
import System.Environment
 
main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Sum: " ++ show ((read (args !! 0) + (read (args !! 1)))))
