module Tokenizer where
import RegexToken(Token(Tk), (<|>), anyCharTokenizer, whitespaceTokenizer, inlineCommentTokenizer, blockCommentTokenizer, keywordTokenizer)

exhaustiveTokenizer :: (String -> (Maybe Token, String)) -> String -> [Token]
exhaustiveTokenizer tokenizer input =
    if length input == 0
    then []
    else token:(exhaustiveTokenizer tokenizer remainingText)
    where
        (Just token, remainingText) = tokenizer input

commentFilter :: String -> String
commentFilter input = map (\(Tk _ (c:_)) -> c) validTokens
    where
        allTokens = (exhaustiveTokenizer (anyCharTokenizer <|> inlineCommentTokenizer <|> blockCommentTokenizer)) input
        validTokens = (filter (\(Tk tag value) -> length value == 1) allTokens)

test = "//lolwtf\n    i want to say        /* sdfe lol \n \r * skdfje wtf */ something"

