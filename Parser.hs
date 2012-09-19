module Parser where


data Rule = Rule {root::String, production::[String]} deriving (Show, Eq)
data Symbol = Terminal Grammar String | NonTernimal Grammar String deriving (Show, Eq)

type Grammar = [Rule]
type SymbolTable = [Symbol]
{-
  -- Circular dependency of types
  data Terminal = Terminal String
  data NonTernimal = NonTernimal String [[Symbol]]
  data Symbol = Terminal | NonTerminal
-}

name :: Symbol -> String
name (Terminal _ symbolName) = symbolName
name (NonTernimal _ symbolName) = symbolName

symbolTable :: Grammar -> SymbolTable
symbolTable grammar = terminalList ++ nonTerminalList
  where
    fullSymbolNameList = foldl (\symbolNameList symbolName -> if length [s | s <- symbolNameList, s == symbolName] > 0 then symbolNameList else symbolName:symbolNameList) [] (concat [(root rule):production rule | rule <- grammar])
    nonTerminalList = [NonTernimal grammar (root rule) | rule <- grammar]    
    terminalList = [Terminal grammar symbolName | symbolName <- fullSymbolNameList, length [symbol | symbol <- nonTerminalList, name symbol == symbolName] == 0]

productions :: Symbol -> [[Symbol]]
productions (Terminal _ _) = []
productions (NonTernimal grammar symbolName) = [[head [symbol | symbol <- table, name symbol == symbolName] | symbolName <- production rule] | rule <- relevantRules]
  where
    table = symbolTable grammar
    relevantRules = [rule | rule <- grammar, root rule == symbolName]
    relevantSymbolNames = [[symbolName | symbolName <- production rule] | rule <- relevantRules]

getNullableList :: SymbolTable -> [Symbol]
getNullableList table = symbols
    where
        symbols = table

resolveProduction :: [Symbol] -> [(Symbol, [Symbol])] -> [Symbol] -> Maybe [Symbol]
resolveProduction nullable firstList [] = Just []
resolveProduction nullable firstList (first:rest) = case (length theNullableList > 0, nextFirstMaybe, length theFirstList > 0) of
                                                    (False, _, True) -> Just theFirst
                                                    (True, Just nextFirst, True) -> Just (theFirst ++ nextFirst)
                                                    --(True, Nothing, _) -> Nothing
                                                    --(_, _, False) -> Nothing
                                                    _   -> Nothing
  where
    theNullableList = [symbol | symbol <- nullable, symbol == first]
    nextFirstMaybe = resolveProduction nullable firstList rest
    theFirstList = [theFirst | (symbol, theFirst) <- firstList, symbol == first]
    theFirst = head theFirstList
    


buildFirstList :: [Symbol] -> [(Symbol, [Symbol])] -> Symbol -> [(Symbol, [Symbol])]
buildFirstList nullable firstList (Terminal grammar symbolName) = (symbol, [symbol]) : firstList
  where
    symbol = Terminal grammar symbolName
buildFirstList nullable firstList (NonTernimal grammar symbolName) =
  if foldl (\isResolvable firstFromProduction -> if isResolvable then firstFromProduction /= Nothing else False) True firstFromProductionList
  then (symbol, mergedFirstFromProductionList):firstList
  else firstList
  where
    symbol = NonTernimal grammar symbolName
    productionList = productions symbol
    firstFromProductionList = map (resolveProduction nullable firstList) productionList
    mergedFirstFromProductionList = foldl (\mergedFirst (Just firstFromProduction) -> mergedFirst ++ firstFromProduction) [] firstFromProductionList

{- checking for left recursion is necessary -}
{-
getFirstList :: Grammar -> [(Symbol, [Symbol]]
getFirstList = [Symbol]
  where
    rules = grammar

getFollowList :: Grammar -> [Symbol]
getFollowList [Symbol]
-}
