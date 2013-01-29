module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                  Left err -> "No match: " ++ show err
                  Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
 
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
 
 
 
 
 
