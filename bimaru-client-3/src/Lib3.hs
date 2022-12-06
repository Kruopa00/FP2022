{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document (DMap, DList, DInteger, DString, DNull), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.Either ()
import GHC.Base ()
import Data.Bool ()
import Data.Char
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)


friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument stri = do
    str <- removeDashes stri     -- removes ---\n from the front if any
    (str', flag) <- checkChar '-' str
    if flag then do     -- DList or negative 
        (_, flag') <- checkChar ' ' str'
        if flag' then do     -- DList
            (doc, _) <- ultimateParser3000 (DList []) str 0
            return doc
        else do     -- negative single
            (a, _) <- parseStringUntil '\n' "" str
            return (convertSingleToDoc a)
    else do     -- DMap or single
        if (str == " ") then do
            (return (DString " "))
        else do
            (a, _) <- parseStringUntil '\n' "" str
            (_, flag') <- checkCharRecursive ':' a
            if flag' then do     -- DMap
                (doc, _) <- ultimateParser3000 (DMap []) str 0
                return doc
            else        -- single
                return (convertSingleToDoc a)
parseDocument _ = Left "Not S"

ultimateParser3000 :: Document -> String -> Integer-> Either String (Document, String)
ultimateParser3000 d "" _ = do
    return (d, "")
ultimateParser3000 (DMap d) s sk = do
    (ss1, _) <- parseStringUntil '\n' "" s
    (_, flag) <- checkCharRecursive ':' ss1
    if flag then do
        (aa, b) <- parseStringUntil ':' "" s     -- paimam a:b, a yra stringas, b document
        (a, _) <- parseSpace 0 aa
        (b1, flag') <- checkChar ' ' b
        if flag' then do     -- single
            (k, l) <- parseStringUntil '\n' "" b1       -- k yra likęs tuplo vidus, l yra visi likę tuplai
            (_, spaceCount) <- parseSpace 0 l
            if (spaceCount < sk) then
                return (DMap (d ++ [(remove' a, convertSingleToDoc k)]), l)
            else
                ultimateParser3000 (DMap (d ++ [(remove' a, convertSingleToDoc k)])) l sk
        else do     -- listas
            (l, flagType) <- parseUntilPlural b1
            if (flagType) then do   -- jei true, tai DListas
                (d1, b2) <- ultimateParser3000 (DList []) l (sk)
                (sss1, _) <- parseStringUntil '\n' "" b2
                (_, flag1) <- checkCharRecursive ':' sss1
                --(s3, s4) <- parseStringUntil ':' "" sss1
                if flag1 then do
                    (_, spaceCount) <- parseSpace 0 sss1
                    if (spaceCount < sk) then
                        return (DMap (d ++ [((remove' a), d1)]), b2)
                    else
                        ultimateParser3000 (DMap (d ++ [(remove' a, d1)])) b2 sk
                else do
                    (_, spaceCount) <- parseSpace 0 sss1
                    if (spaceCount < sk) then do
                        return (DMap (d ++ [(remove' a, d1)]), b2)
                    else
                        ultimateParser3000 (DMap (d ++ [(remove' a, d1)])) b2 sk                
            else do     -- DMap
                (d3, s3) <- ultimateParser3000 (DMap []) l (sk + 2)
                (_, spaceCount) <- parseSpace 0 s3
                if (spaceCount < sk) then
                    return (DMap (d ++ [(remove' a, d3)]), s3)
                else
                    ultimateParser3000 (DMap (d ++ [(remove' a, d3)])) s3 sk
    else
        return (DMap d, s)
ultimateParser3000 (DList d) str sk = do
    (s, _) <- parseSpace 0 str
    (_, flag) <- checkChar '-' s
    if (not flag) then do
        return (DList d, str)
    else do
        (_, s1) <- parseStringUntil ' ' "" s
        (_, flagType) <- parseUntilPlural s1
        if (flagType == True) then do       --Dlistas dliste
            (d1, s1') <- ultimateParser3000 (DList []) s1 (sk + 2)
            (_, inte) <- parseSpace 0 s1'
            if (inte < sk) then
                return (DList (d ++ [d1]), s1')
            else
                ultimateParser3000 (DList (d ++ [d1])) s1' sk
        else do         -- ateis arba DMapas, arba single reikšmė
            (ss1, s2) <- parseStringUntil '\n' "" s1
            (_, flag') <- checkCharRecursive ':' ss1
            (s3, _) <- parseStringUntil ':' "" ss1
            if (flag' == False) then do      -- single
                (_, spaceCount) <- parseSpace 0 s2
                (sss1, _) <- parseStringUntil '\n' "" s2
                (_, flag1) <- checkCharRecursive ':' sss1
                (_, flagType') <- parseUntilPlural sss1
                if (spaceCount < sk) then
                    return (DList (d ++ [(convertSingleToDoc s3)]), s2)                
                else if (spaceCount <= sk && flag1 && (flagType' == False)) then
                    return (DList (d ++ [(convertSingleToDoc s3)]), s2) 
                else
                    ultimateParser3000 (DList (d ++ [(convertSingleToDoc s3)])) s2 spaceCount
            else do     -- DMap
                (dmap, string) <- ultimateParser3000 (DMap []) s1 (sk + 2)
                (str2, spaceCount') <- parseSpace 0 string
                (_, flag1) <- checkChar '-' str2
                if flag1 then do
                    if (spaceCount' < sk) then
                        return (DList ((d ++ [dmap])), string)
                    else
                        ultimateParser3000 (DList (d ++ [dmap])) string spaceCount'
                else do
                    return (DList ((d ++ [dmap])), string)

ultimateParser3000 _ _ _ = Left "Blogai ultimateParser3000"

removeFour :: String -> Integer -> String
removeFour (x:xs) n = do
    if (n > 3) then do
        x:xs
    else do
        removeFour xs (n + 1)
removeFour x _ = x

removeDashes :: String -> Either String String
removeDashes x
    | take 4 x == "---\n" = Right $ removeFour x 0
    | otherwise = Right x
removeDashes _ = Left "Blogai"

remove' :: String -> String
remove' (x:xs) = do
    if x == '\'' then (take ((Prelude.length xs) - 1) xs)
    else do
        (x:xs)
remove' x = x




convertSingleToDoc :: String -> Document
convertSingleToDoc s
    | s == "' '\n" = DString " "
    | s == "' '" = DString " "
    | s == "''" = DString ""
    | s == "\n" = DString ""
    | s == "''\n" = DString ""
    | s == "[]" = DList []
    | s == "{}" = DMap []
    | s == "[]\n" = DList []
    | s == "{}\n" = DMap []
    | s == "" = DString ""
    | s == "null" = DNull
    | take 1 s == "'" = (DString $ take ((Prelude.length (drop 1 s)) - 1) (drop 1 s))
    | isNegNumber s = DInteger (read s)
    | isNumber' s = DInteger (read s)
    | not (isNumber' s) = DString s
    | otherwise = DString s

isNumber' :: String -> Bool
isNumber' "" = False
isNumber' (x:xs)
    | isDigit x && xs /= "" = isNumber' xs
    | isDigit x && xs == "" =  True
    | otherwise = False


isNegNumber :: String -> Bool
isNegNumber (x:xs)
    | ((x == '-') && (isNumber' xs)) = True
    | otherwise = False
isNegNumber _ = False

parseStringUntil :: Char -> String -> String -> Either String (String, String)
parseStringUntil _ s "" = Right (s, "")
parseStringUntil ch s (x:xs) | ch == x = Right (s, xs)
                    | otherwise = parseStringUntil ch (s ++ [x]) xs
parseStringUntil _ _ _ = Left "Empty"

parseUntilPlural :: String -> Either String (String, Bool)                -- jei Dlistas true, jei dmapas false
parseUntilPlural (x:xs)
    | x == '-' = do
        if (take 1 xs /= " ") then
            return (x:xs, False)
        else
            Right (x:xs, True)
    | isDigit x = Right (x:xs, False)
    | isLetter x = Right (x:xs, False)
    | x=='\'' = Right (x:xs, False)
    | x=='[' = Right (x:xs, False)
    | x=='{' = Right (x:xs, False)
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
gameStart (State l) (GameStart d)
    | State l == State [("Initial state",DNull)] = State $ ("Game", DList [DMap [("occupied_cells", DList [])], (DMap (("test", DNull):(reverse d)))]) : l
    | State l /= State [("Initial state",DNull)] = State []
gameStart _ (GameStart []) = State []
gameStart _ _ = State []

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
    --fromDocument x = Left (show x)
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



