{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DString, DNull), Check(..), Coord(..), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.Either (Either(Right, Left))
import GHC.Base (error, when)
import Data.Bool (Bool(True, False))
import Data.Char

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = do
    (_, flag)<- checkChar '-' str
    if flag then do
        (doc, _) <- ultimateParser3000 (DList []) str 0
        return doc
    else do
        (a, _) <- parseStringUntil '\n' "" str
        (_, flag) <- checkCharRecursive ':' a
        if flag then do
            (doc, _) <- ultimateParser3000 (DMap []) str 0
            return doc
        else
            return (convertSingleToDoc a) 
parseDocument _ = Left "Implement me"

ultimateParser3000 :: Document -> String -> Integer-> Either String (Document, String)
ultimateParser3000 d "" sk = do
    return (d, "") 
ultimateParser3000 (DMap d) s sk = do
    (a, b) <- parseStringUntil ':' "" s         -- paimam a:b, a yra stringas, b document
    (a, _) <- parseSpace 0 a
    --when (a == "coords") $ Left $ show s
    (b1, flag) <- checkChar ' ' b
    if flag then do       -- single reiksme
        (k, l) <- parseStringUntil '\n' "" b1          -- k yra likęs tuplo vidus, l yra visi likę tuplai
        (_, spaceCount) <- parseSpace 0 l
        if (spaceCount < sk) then
            return (DMap (d ++ [(a, convertSingleToDoc k)]), l)
        else
            ultimateParser3000 (DMap (d ++ [(a, convertSingleToDoc k)])) l sk
    else do         -- listas
        (l, flagType) <- parseUntilPlural b1
        if (flagType) then do       -- jei true, tai DListas
            (d1, b2) <- ultimateParser3000 (DList []) l sk
            (ss1, s2) <- parseStringUntil '\n' "" b2
            (_, flag) <- checkCharRecursive ':' ss1
            (s3, s4) <- parseStringUntil ':' "" ss1
            if flag then do
                (_, spaceCount) <- parseSpace 0 ss1
                if (spaceCount < sk) then
                    return (DMap (d ++ [(a, d1)]), b2)
                else 
                    ultimateParser3000 (DMap (d ++ [(a, d1)])) b2 sk
            else
                return (DMap (d ++ [(a, d1)]), b2)
        else do
            (_, spaceCount) <- parseSpace 0 b1
            (d3, s3) <- ultimateParser3000 (DMap []) l (sk + 2)
            --Left $ show d3
            ultimateParser3000 (DMap (d ++ [(a, d3)])) s3 sk            

ultimateParser3000 (DList d) str sk = do
    (s, spaceCount) <- parseSpace 0 str
    (_, flag) <- checkChar '-' s
    if (not flag) then do
        return (DList d, s)
    else do
        (_, s1) <- parseStringUntil ' ' "" s
        (_, flagType) <- parseUntilPlural s1
        if (flagType == True) then do
            --Dlistas dliste
            (d1, s1) <- ultimateParser3000 (DList []) s1 (sk + 2)
            (_, inte) <- parseSpace 0 s1
            if (inte < sk) then
                return (DList (d ++ [d1]), s1)
            else
                ultimateParser3000 (DList (d ++ [d1])) s1 sk
            
        else do         -- ateis arba DMapas, arba single reikšmė
            (ss1, s2) <- parseStringUntil '\n' "" s1
            (_, flag) <- checkCharRecursive ':' ss1
            (s3, s4) <- parseStringUntil ':' "" ss1
            if (flag == False) then do
                (_, spaceCount) <- parseSpace 0 s2
                if (spaceCount < sk) then
                    return (DList (d ++ [(convertSingleToDoc s3)]), s2)
                else
                    ultimateParser3000 (DList (d ++ [(convertSingleToDoc s3)])) s2 spaceCount
                -- reiškia s3 single reikšmė
            else do
                (dmap, s) <- ultimateParser3000 (DMap []) s1 (sk + 2)     -- ateina DMap
                (_, spaceCount') <- parseSpace 0 s
                if (spaceCount' < sk) then
                    return (DList (d ++ [dmap]), s)
                else
                    ultimateParser3000 (DList (d ++ [dmap])) s sk

ultimateParser3000 _ _ _ = Left "Blogai ultimateParser3000"
    --return DString "a"

convertSingleToDoc :: String -> Document
convertSingleToDoc s 
    | s == "null" = DNull                                         -- nežinom kaip yamle žymimas null
    | isNumber' s = DInteger (read s)
    | not (isNumber' s) = DString s

isNumber' :: String -> Bool
isNumber' "" = False
isNumber' (x:xs)
    | isDigit x && xs /= "" = isNumber' xs
    | isDigit x && xs == "" =  True
    | otherwise = False


parseStringUntil :: Char -> String -> String -> Either String (String, String)
parseStringUntil ch s "" = Right (s, "")
parseStringUntil ch s (x:xs) | ch == x = Right (s, xs)
                    | otherwise = parseStringUntil ch (s ++ [x]) xs
parseStringUntil ch _ _ = Left "Empty"

parseUntilPlural :: String -> Either String (String, Bool)                -- jei Dlistas true, jei dmapas false
parseUntilPlural (x:xs) 
    | x == '-' = Right (x:xs, True)
    | isDigit x = Right (x:xs, False)
    | isLetter x = Right (x:xs, False)
    | otherwise = parseUntilPlural xs
parseUntilPlural "" = Right ("", False)
parseUntilPlural _ = Left "blogai"

checkChar :: Char -> String -> Either String (String, Bool)
checkChar ch (x:xs) | ch == x = Right (xs, True)
                    | otherwise = Right (xs, False)
checkChar _ "" = Right ("", False)

checkCharRecursive :: Char -> String -> Either String (String, Bool)
checkCharRecursive ch (x:xs) | ch == x = Right (xs, True)
                    | otherwise = checkCharRecursive ch xs
checkCharRecursive _ "" = Right ("", False)

parseSpace :: Integer -> String -> Either String (String, Integer)
parseSpace num "" = Right ("", num)
parseSpace num (x:xs)   | ' ' == x = parseSpace (num + 1) xs
                        | otherwise = Right ((x:xs), num)
parseSpace x _ = Right ("", x)
                
-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument

data GameStart = GameStart [(String, Document)] 
    deriving (Show, Eq)

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
--gameStart x (GameStart y) = error $ (show x) ++ "\nLAB" ++ (show (("test", DNull):(reverse y)))
gameStart _ (GameStart []) = State []
gameStart (State l) (GameStart d)
    | State l == State [("Initial state",DNull)] = State $ ("Game", DList [DMap [("occupied_cells", DList [])], (DMap (("test", DNull):(reverse d)))]) : l
    | State l /= State [("Initial state",DNull)] = State []


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint [(String, Document)] 
    deriving (Show, Eq)

instance FromDocument Hint where
    fromDocument :: Document -> Either String Hint 
    --fromDocument x = Left $ (show x) ++ "1"
    fromDocument (DMap x) = Right (Hint x)
    fromDocument _ = Left "No"

instance FromDocument GameStart where
    fromDocument :: Document -> Either String GameStart
    --fromDocument (DMap x) = Left $ (show (reverse x)) ++ "2"
    fromDocument (DMap x) = Right (GameStart x)
    fromDocument _ = Left "No"

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State

hint _ (Hint[(_,DNull)]) = State []
hint _ (Hint[(string,_)]) 
    | string /= "coords" = State []
hint (State[]) _ = State []
hint (State (l:ls)) (Hint t) = State (hintFunc1 l (DMap t) : ls)

hintFunc1 :: (String, Document) -> Document -> (String, Document)
hintFunc1 (l,ls) t = (l, hintFunc2 ls t)

hintFunc2 :: Document -> Document -> Document
hintFunc2 (DList (l:ls)) t = DList (hintFunc3 l t:ls)
hintFunc2 _ _ = DList []

hintFunc3 :: Document -> Document -> Document
hintFunc3 (DMap (l:ls)) t = DMap (hintFunc4 l t :ls)
hintFunc3 _ _ = DMap []

hintFunc4 :: (String, Document) -> Document -> (String, Document)
hintFunc4 (l, ls) t = (l, hintFunc5 ls t)

hintFunc5 :: Document -> Document -> Document
hintFunc5 (DList l) t = DList (hintFunc6 l t)
hintFunc5 _ _ = DList []

hintFunc6 :: [Document] -> Document -> [Document]
hintFunc6 l t = hintFunc7 t:l

hintFunc7 :: Document -> Document
hintFunc7 (DMap [l]) = hintFunc8 l
hintFunc7 _ = DMap []

hintFunc8 :: (String, Document) -> Document
hintFunc8 (_,ls) =  hintFunc9 ls

hintFunc9 :: Document -> Document
hintFunc9 (DMap [l,(_,DNull)]) = hintFunc10 l
hintFunc9 (DMap [_,(ls,lss)]) = hintFunc8 (ls,lss)
hintFunc9 _ = DMap []

hintFunc10 :: (String, Document) -> Document
hintFunc10 (_,ls) = ls

