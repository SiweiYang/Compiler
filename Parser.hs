module Parser where

import Text.Printf(printf)
import Data.List(sortBy)

data Rule = Rule {root::String, production::[String]} deriving (Show, Eq)
data Symbol = Terminal Grammar String | NonTerminal Grammar String deriving (Eq)
instance Show Symbol where
  show (Terminal _ symbolName) = printf "Terminal[%s]" symbolName
  show (NonTerminal _ symbolName) = printf "NonTerminal[%s]" symbolName

type Grammar = [Rule]
type SymbolTable = [Symbol]
type FirstTable = [(Symbol, [Symbol])]
{-
  -- Circular dependency of types
  data Terminal = Terminal String
  data NonTernimal = NonTernimal String [[Symbol]]
  data Symbol = Terminal | NonTerminal
-}

name :: Symbol -> String
name (Terminal _ symbolName) = symbolName
name (NonTerminal _ symbolName) = symbolName

symbolTable :: Grammar -> SymbolTable
symbolTable grammar = terminalList ++ nonTerminalList
  where
    fullSymbolNameList = foldl (\symbolNameList symbolName -> if length [s | s <- symbolNameList, s == symbolName] > 0 then symbolNameList else symbolName:symbolNameList) [] (concat [(root rule):production rule | rule <- grammar])
    nonTerminalList = [NonTerminal grammar symbolName | symbolName <- fullSymbolNameList, length [rule | rule <- grammar, root rule == symbolName] /= 0]
    terminalList = [Terminal grammar symbolName | symbolName <- fullSymbolNameList, length [rule | rule <- grammar, root rule == symbolName] == 0]

productions :: Symbol -> [[Symbol]]
productions (Terminal grammar symbolName) = [[Terminal grammar symbolName]]
productions (NonTerminal grammar symbolName) = [[head [symbol | symbol <- table, name symbol == symbolName] | symbolName <- production rule] | rule <- relevantRules]
  where
    table = symbolTable grammar
    relevantRules = [rule | rule <- grammar, root rule == symbolName]
    relevantSymbolNames = [[symbolName | symbolName <- production rule] | rule <- relevantRules]

joinSymbolWithSymbols :: Symbol -> [Symbol] -> [[Symbol]]
joinSymbolWithSymbols symbol symbols = [[product | product <- production, elem product symbols] | production <- productions symbol]

buildNotNullableListRecursive :: SymbolTable -> [Symbol] -> [Symbol]
buildNotNullableListRecursive table notNullable = if length notNullable == length mergedNotNullable then notNullable else buildNotNullableListRecursive table mergedNotNullable
  where
    weightedJoinList = [(minimum (map length (joinSymbolWithSymbols symbol notNullable)), symbol) | symbol <- table]
    newNotNullable = [symbol | (weight, symbol) <- weightedJoinList, weight > 0]
    mergedNotNullable = foldl (\symbols symbol -> if elem symbol symbols then symbols else symbol:symbols) notNullable newNotNullable

getNullableList :: SymbolTable -> [Symbol]
getNullableList table = [symbol | symbol <- table, not (elem symbol notNullable)]
  where
    notNullable = buildNotNullableListRecursive table [Terminal grammar symbolName | Terminal grammar symbolName <- table]
        

resolveProduction :: [Symbol] -> FirstTable -> [Symbol] -> Maybe [Symbol]
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

buildFirstList :: [Symbol] -> FirstTable -> Symbol -> FirstTable
buildFirstList nullable firstList (Terminal grammar symbolName) = (symbol, [symbol]) : firstList
  where
    symbol = Terminal grammar symbolName
buildFirstList nullable firstList (NonTerminal grammar symbolName) =
  if foldl (\isResolvable firstFromProduction -> if isResolvable then firstFromProduction /= Nothing else False) True firstFromProductionList
  then (symbol, mergedFirstFromProductionList):firstList
  else firstList
  where
    symbol = NonTerminal grammar symbolName
    productionList = productions symbol
    firstFromProductionList = map (resolveProduction nullable firstList) productionList
    mergedFirstFromProductionList = foldl (\mergedFirst (Just firstFromProduction) -> mergedFirst ++ firstFromProduction) [] firstFromProductionList

buildFirstListRecursive :: [Symbol] -> FirstTable -> [Symbol] -> FirstTable
buildFirstListRecursive nullable firstList symbols = case length newFirstList > length firstList of
                                                      True -> buildFirstListRecursive nullable firstList newSymbols
                                                      False -> firstList
  where
    potentialFirstList = [(buildFirstList nullable firstList symbol, symbol) | symbol <- symbols] :: [(FirstTable, Symbol)]
    newFirstPair = head (sortBy (\(listA, _) (listB, _) -> compare (length listB) (length listA)) potentialFirstList)
    (newFirstList, newFirstSymbol) = newFirstPair
    newSymbols = [symbol | symbol <- symbols, symbol /= newFirstSymbol]

getFirstList :: Grammar -> FirstTable
getFirstList grammar = buildFirstListRecursive nullable [] table
  where
    table = symbolTable grammar
    nullable = getNullableList table
    --(\x -> f x x) (\x -> f x x)
{- checking for left recursion is necessary -}
{-

getFollowList :: Grammar -> [Symbol]
getFollowList [Symbol]
-}
