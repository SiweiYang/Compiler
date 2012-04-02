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
    if result /= Nothing && length rest < length newRest
    then (result, newRest)
    else (Just (Tk regex value), rest)
    where
        input = (value++rest)
        (result, newRest) = tokenizer input
        Just (Tk _ newValue) = result 

makeMaximalTokenizer :: String -> (Maybe Token, String) -> (Maybe Token, String)
makeMaximalTokenizer regex = maximalTokenizerConbinator (makeRegexTokenizer regex)


inlineComment = mkRegex "//.*\n"
blockComment = mkRegexWithOpts "/\\*([^\\*]*\\*)([^/][^\\*]*\\*+)*/" False True
space = mkRegexWithOpts "[ \t\n\r\f\v]+" False True

filterComments :: String -> [String]
filterComments [] = []
filterComments program =
    case (matchInline, matchBlock) of
        (Nothing, Nothing)  -> []
        (_, Nothing)        -> preInline:(filterComments afterInline)
        (Nothing, _)        -> preBlock:(filterComments afterBlock)
        _                   -> case compare (length preInline) (length preBlock) of
                                LT -> preInline:(filterComments afterInline)
                                GT -> preBlock:(filterComments afterBlock)
                                EQ -> [program]
    where
        matchInline = matchRegexAll inlineComment program
        Just (preInline, _, afterInline, _) = matchInline
        matchBlock = matchRegexAll blockComment program
        Just (preBlock, _, afterBlock, _) = matchBlock


test = "//lolwtf\n            /* sdfe lol \n \r * skdfje wtf */"

