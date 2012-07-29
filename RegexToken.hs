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

keywordTokenizerList =
  [
    (makeRegexTokenizer "boolean"       "boolean"),
    (makeRegexTokenizer "byte"          "byte"),
    (makeRegexTokenizer "double"        "double"),
    (makeRegexTokenizer "float"         "float"),
    (makeRegexTokenizer "int"           "int"),
    (makeRegexTokenizer "long"          "long"),
    (makeRegexTokenizer "short"         "short"),

    (makeRegexTokenizer "abstract"      "abstract"),
    (makeRegexTokenizer "extends"       "extends"),
    (makeRegexTokenizer "implements"    "implements"),
    (makeRegexTokenizer "final"         "final"),
    (makeRegexTokenizer "static"        "static"),
    (makeRegexTokenizer "void"          "void"),
    (makeRegexTokenizer "class"         "class"),
    (makeRegexTokenizer "interface"     "interface"),
    (makeRegexTokenizer "const"         "const"),
    (makeRegexTokenizer "synchronized"  "synchronized"),

    (makeRegexTokenizer "new"           "new"),
    (makeRegexTokenizer "this"          "this"),
    (makeRegexTokenizer "super"         "super"),

    (makeRegexTokenizer "private"       "private"),
    (makeRegexTokenizer "protected"     "protected"),
    (makeRegexTokenizer "public"        "public"),

    (makeRegexTokenizer "switch"        "switch"),
    (makeRegexTokenizer "case"          "case"),
    (makeRegexTokenizer "default"       "default"),

    (makeRegexTokenizer "while"         "while"),
    (makeRegexTokenizer "for"           "for"),
    (makeRegexTokenizer "goto"          "goto"),
    (makeRegexTokenizer "return"        "return"),
    (makeRegexTokenizer "continue"      "continue"),
    (makeRegexTokenizer "do"            "do"),
    (makeRegexTokenizer "else"          "else"),
    (makeRegexTokenizer "if"            "if"),

    (makeRegexTokenizer "throw"         "throw"),
    (makeRegexTokenizer "throws"        "throws"),
    (makeRegexTokenizer "try"           "try"),
    (makeRegexTokenizer "catch"         "catch"),
    (makeRegexTokenizer "finally"       "finally"),

    (makeRegexTokenizer "import"       "import"),
    (makeRegexTokenizer "instanceof"       "instanceof"),
    (makeRegexTokenizer "package"       "package"),

    (makeRegexTokenizer "volatile"      "volatile"),
    (makeRegexTokenizer "transient"       "transient"),
    (makeRegexTokenizer "strictfp"       "strictfp"),
  ]
keywordTokenizer = makeRegexTokenizer "keyword" "(abstract)|(assert)|(boolean)|(break)|(byte)|(case)|(catch)|(char)|(class)|(const)|(continue)|(default)|(do)|(double)|(else)|(enum)|(extends)|(final)|(finally)|(float)|(for)|(goto)|(if)|(implements)|(import)|(instanceof)|(int)|(interface)|(long)|(native)|(new)|(package)|(private)|(protected)|(public)|(return)|(short)|(static)|(strictfp)|(super)|(switch)|(synchronized)|(this)|(throw)|(throws)|(transient)|(try)|(void)|(volatile)|(while)"

symbolTokenizerList =
  [
    (makeRegexTokenizer "LPAREN"  "["),
    (makeRegexTokenizer "RPAREN"  "]"),
    (makeRegexTokenizer "LCURLY"  "{"),
    (makeRegexTokenizer "RCURLY"  "}"),
    (makeRegexTokenizer "ASSIGN"  "="),
    (makeRegexTokenizer "EQUALS"  "=="),
    (makeRegexTokenizer "LT"      "<"),
    (makeRegexTokenizer "GT"      "["),
    (makeRegexTokenizer "MINUS"   "-"),
    (makeRegexTokenizer "PLUS"    "+"),
    (makeRegexTokenizer "DIVIDE"  "+"),
    (makeRegexTokenizer "TIMES"   "*"),
    (makeRegexTokenizer "MOD"     "%"),
    (makeRegexTokenizer "MINUS1"  "--"),
    (makeRegexTokenizer "PLUS1"   "++"),
    (makeRegexTokenizer "COLON"   ":"),
    (makeRegexTokenizer "SCOLON"  ";"),
    (makeRegexTokenizer "SQUOTE"  "'"),
    (makeRegexTokenizer "DQUOTE"  "\""),
    (makeRegexTokenizer "BAND"  "&"),
    (makeRegexTokenizer "LAND"  "&&"),
    (makeRegexTokenizer "BOR"  "\\|"),
    (makeRegexTokenizer "LOR"  "\\|\\|")
  ]
symbolTokenizer = makeRegexTokenizer "symbol" "([)|(])|({)|(})|(=)|(==)|(<)|(>)|(-)|(--)|(+)|(++)|(:)|(;)|(\")|(')|(%)|(^)|(&)|(&&)|(*)|(/)|(\\|)|(\\|\\|)"