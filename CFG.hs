--module CFG where
import System.Environment(getArgs)
import System.IO(openFile, IOMode(ReadMode), Handle, hIsEOF, hGetLine, hPutStrLn)

import Text.Regex(mkRegex, splitRegex)
import Parser(Rule(Rule), symbolTable, getNullableList, getFirstList)
import Parser(Symbol(Terminal), productions, joinSymbolWithSymbols)

type FilePath = String

readLines :: Handle -> IO [String]
readLines h = do
  isEOF <- hIsEOF h
  if isEOF
  then return []
  else do
    line <- hGetLine h
    lines <- readLines h
    return (line:lines)

split :: String -> String -> [String]
split expr = splitRegex regex
  where
    regex = mkRegex expr

main :: IO ()
main = do
  filename:_ <- getArgs
  file <- openFile filename ReadMode
  putStrLn filename
  lines <- readLines file
  let tokens = [split " " line | line <- lines]
  let grammar = [Rule (head token) (drop 2 token) | token <- tokens, length token > 1]
  let table = symbolTable grammar
  let notNullableV0 = [Terminal grammar symbolName | Terminal grammar symbolName <- table]
  let notNullableV1Pair = [(minimum (map length (joinSymbolWithSymbols symbol notNullableV0)), symbol) | symbol <- table]
  let notNullableV1 = [symbol | (weight, symbol) <- notNullableV1Pair, weight > 0]
  let notNullableV2Pair = [(minimum (map length (joinSymbolWithSymbols symbol notNullableV1)), symbol) | symbol <- table]
  let notNullableV2 = [symbol | (weight, symbol) <- notNullableV2Pair, weight > 0]
  print table
  print [(symbol, productions symbol) | symbol <- table]
  print notNullableV0
  print notNullableV1Pair
  print notNullableV1
  print notNullableV2Pair
  print notNullableV2
  
  print (getNullableList table)
  print (getFirstList grammar)