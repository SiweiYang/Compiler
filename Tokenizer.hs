module Tokenizer where
import Text.Regex (mkRegex, mkRegexWithOpts, matchRegex, matchRegexAll, splitRegex)

data Token = Tk String String deriving (Show, Eq)

makeRegexTokenizer :: String -> (String -> (Maybe Token, String))
makeRegexTokenizer regex =
    (\input -> 
        case matchRegexAll pattern input of
            Nothing -> (Nothing, input)
            Just (_, value, rest, _) -> (Just (Tk regex value), rest)
        )
    where
        pattern = mkRegexWithOpts ('^':regex) False True

maximalTokenizerConbinator :: (String -> (Maybe Token, String)) -> (Maybe Token, String) -> (Maybe Token, String)
maximalTokenizerConbinator tokenizer (Nothing, input) = tokenizer input
maximalTokenizerConbinator tokenizer (Just (Tk regex value), rest) =
    if result /= Nothing && length rest > length newRest
    then (result, newRest)
    else (Just (Tk regex value), rest)
    where
        input = (value++rest)
        (result, newRest) = tokenizer input
        Just (Tk _ newValue) = result 

makeMaximalTokenizer :: String -> (Maybe Token, String) -> (Maybe Token, String)
makeMaximalTokenizer regex = maximalTokenizerConbinator (makeRegexTokenizer regex)

--inlineComment = mkRegex "//[^\n]*\n"
--blockComment = mkRegexWithOpts "/\\*([^\\*]*\\*)([^/][^\\*]*\\*+)*/" False True
--space = mkRegexWithOpts "[ \t\n\r\f\v]+" False True

anyCharTokenizer = makeMaximalTokenizer "."
inlineCommentTokenizer = makeMaximalTokenizer "//[^\n]*\n"
blockCommentTokenizer = makeMaximalTokenizer "/\\*([^\\*]*\\*)([^/][^\\*]*\\*+)*/"

exhaustiveTokenizer :: ((Maybe Token, String) -> (Maybe Token, String)) -> String -> [Token]
exhaustiveTokenizer tokenizer input =
    if length input == 0
    then []
    else token:(exhaustiveTokenizer tokenizer remainingText)
    where
        (Just token, remainingText) = tokenizer (Nothing, input)

commentFilter :: String -> String
commentFilter input = map (\(Tk _ (c:_)) -> c) validTokens
    where
        allTokens = (exhaustiveTokenizer (anyCharTokenizer . inlineCommentTokenizer . blockCommentTokenizer) input)
        validTokens = (filter (\(Tk tag value) -> length value == 1) allTokens)

test = "//lolwtf\n    i want to say        /* sdfe lol \n \r * skdfje wtf */ something"

