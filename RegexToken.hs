module RegexToken where
import Text.Regex (mkRegex, mkRegexWithOpts, matchRegex, matchRegexAll, splitRegex)

data Token = Tk String String deriving (Show, Eq)

makeRegexTokenizer :: String -> String -> (String -> (Maybe Token, String))
makeRegexTokenizer name regex =
    (\input -> 
        case matchRegexAll pattern input of
            Nothing -> (Nothing, input)
            Just (_, value, rest, _) -> (Just (Tk name value), rest)
        )
    where
        pattern = mkRegexWithOpts ('^':regex) False True

(<|>) :: (String -> (Maybe Token, String)) -> (String -> (Maybe Token, String)) -> (String -> (Maybe Token, String))
t1 <|> t2     = (\input ->
                    let
                        (tk1, remain1) = t1 input
                        (tk2, remain2) = t2 input
                    in
                    case (tk1, tk2) of
                        (Nothing, Nothing) -> (Nothing, input)
                        (_, Nothing) -> (tk1, remain1)
                        (Nothing, _) -> (tk2, remain2)
                        _ -> if length remain1 < length remain2 then (tk1, remain1) else (tk2, remain2)
                    )

anyCharTokenizer = makeRegexTokenizer "any" "."

whitespaceTokenizer = makeRegexTokenizer "whitespace" " |\t|\n"

inlineCommentTokenizer = makeRegexTokenizer "inline-comment" "//[^\n]*\n"

{-
    /* -> /\\*
    (any (non *)) + * -> [^\\*]*\\*
    (any ((non /) + (any (non *)) + (many *))) -> ([^/][^\\*]*\\*+)*
    / -> /
-}
blockCommentTokenizer = makeRegexTokenizer "block-comment" "/\\*([^\\*]*\\*)([^/][^\\*]*\\*+)*/"

keywordTokenizer = makeRegexTokenizer "keyword" "(abstract)|(assert)|(boolean)|(break)|(byte)|(case)|(catch)|(char)|(class)|(const)|(continue)|(default)|(do)|(double)|(else)|(enum)|(extends)|(final)|(finally)|(float)|(for)|(goto)|(if)|(implements)|(import)|(instanceof)|(int)|(interface)|(long)|(native)|(new)|(package)|(private)|(protected)|(public)|(return)|(short)|(static)|(strictfp)|(super)|(switch)|(synchronized)|(this)|(throw)|(throws)|(transient)|(try)|(void)|(volatile)|(while)"