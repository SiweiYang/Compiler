--module CFG where
import System.Environment(getArgs)
import System.IO(openFile, IOMode(ReadMode), Handle, hIsEOF, hGetLine, hPutStrLn)

import Text.Regex(mkRegex, splitRegex)
import Parser(Rule(Rule))

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
  print [[Rule (head token) (drop 2 token) | token <- split " " line, length token > 2] | line <- lines]