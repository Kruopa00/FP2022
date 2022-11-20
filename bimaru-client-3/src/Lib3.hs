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
--parseDocument x = Left $ show $ parseStringUntil ':' "" x
parseDocument x = do
    (d, s) <- duhas (DMap []) x
    return d
parseDocument _ = Left "Implement me"

duhas :: Document -> String -> Either String (Document, String)
duhas d "" = do
    return (d, "") 
duhas (DMap d) s = do
    (a, b) <- parseStringUntil ':' "" s                        -- paimam a:b, a yra stringas, b document
    (b1, flag) <- checkChar ' ' b
    if (flag == True) then do
        (k, l) <- parseStringUntil '\n' "" b1                       -- k yra likęs tuplo vidus, l yra visi likę tuplai
        (d3, s3)<- duhas (DMap ((a, convertSingleToDoc k) :d)) l
        return (d3, s3)
        --(k, l) <- parseStringUntil '\n' "" b                        -- k yra likęs tuplo vidus, l yra visi likę tuplai
    else do
        (l, flagType) <- parseUntilPlural b1
        if (flagType) then do       -- jei true, tai DListas
            (d1, b2) <- duhas (DList []) l
            (d3, s3) <- duhas (DMap ((a, d1) :d)) b2
            return (d3, s3)
        else do
            return $ (DString "a", "")
        
duhas (DList d) s = do
    (_, flag) <- checkChar '-' s
    if (not flag) then do
        return (DList d, s)
    else do
        (_, s1) <- parseStringUntil ' ' "" s
        (_, flagType) <- parseUntilPlural s1
        if (flagType == True) then do
            --Dlistas dliste
            (d1, s1) <- duhas (DList []) s1
            (d2, s2) <- duhas (DList (d1:d)) s1
            return $ (d2, s2)
        else do         -- ateis arba DMapas, arba single reikšmė
            (s1, s2) <- parseStringUntil '\n' "" s1
            (s3, s4) <- parseStringUntil ':' "" s1
            if (s4 == "") then do
                duhas (DList ((convertSingleToDoc s3):d)) s2
                -- reiškia s3 single reikšmė
            else do
                -- ateina DMap
                --(dmap, s) <- duhas (DMap []) (s1 ++ s2)
                --(dmap1, s1) <- duhas (DList (dmap:d)) s
                Left "bbd"
duhas _ _ = Left "Blogai"
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
checkChar _ _ = Left "Blogai"
                
-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument

data GameStart = GameStart [(String, Document)] 
    deriving (Show, Eq)

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart _ (GameStart []) = State []
gameStart (State l) (GameStart d)
    | State l == State [("Initial state",DNull)] = State $ ("Game", DList [DMap [("occupied_cells", DList [])], (DMap d)]) : l
    | State l /= State [("Initial state",DNull)] = State []
gameStart _ _ = State []


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint [(String, Document)] 
    deriving (Show, Eq)

instance FromDocument Hint where
    fromDocument :: Document -> Either String Hint 
    fromDocument x = Left $ (show x) ++ "1"
    fromDocument (DMap x) = Right (Hint x)
    fromDocument _ = Left "No"

instance FromDocument GameStart where
    fromDocument :: Document -> Either String GameStart
    fromDocument x = Left $ (show x) ++ "2"
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

