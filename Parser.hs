data Rule = Rule {Root::String, Production::[String]} deriving (Show)
type [Rule] Grammar
data Symbol = Terminal String | NonTernimal [[Symbol]] deriving (Show)


GetSymbolList grammar = [Symbol]
    where
        rules = grammar
GetNullableList = [Symbol]

{- checking for left recursion is necessary -}
GetFirstList = [Symbol]
GetFollowList [Symbol]
