{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart, renderDocumentRecursive) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DString, DNull), Check(..), Coord(..) )
import Lib1 (State(..))
import Data.Char (isDigit)


instance ToDocument Check where
    toDocument (Check x) = DMap [("coords",DList (check' x []))]


check' :: [Coord] -> [Document] ->  [Document]
check' (x:xs) dab
    | xs /= [] = check' xs ((coordGet x) : dab)
    | xs == [] = coordGet x : dab
check' _ _ = []

coordGet :: Coord -> Document
coordGet (Coord x y) = DMap[("col",DInteger x),("row",DInteger y)]


removenl :: String -> Char -> String -> String
removenl (x:xs) c g = do
    if ( x == '\n' && c == '\n') then do
        removenl xs x g
    else 
        removenl xs x (g++[x])
removenl (x:[]) c g = do
    if ( x == '\n' && c == '\n') then
        (g ++ "1")
    else 
        ((g ++ [x])++"2")
removenl _ _ g = g 



removeSpace :: String -> String
removeSpace str = do 
    if drop ((length str) - 2) str == "  " then
        removeSpace (take ((length str) - 2) str)
    else 
        str 

renderDocument :: Document -> String
renderDocument x =  (removenl ("---\n" ++  (renderDocumentRecursive x 0 "")) 'c' "")

renderDocumentRecursive :: Document -> Int -> String -> String

renderDocumentRecursive (DList []) _ string = do
    string ++ "[]"

renderDocumentRecursive (DMap []) _ string = do
    string ++ "{}"

renderDocumentRecursive (DMap x) c string = do
    renderMap (DMap x) c string

renderDocumentRecursive (DList x) c string = do
    renderList (DList x) c (string)

renderDocumentRecursive (DInteger x) _ string = do
    string ++ show x

renderDocumentRecursive (DString x) _ string = do
    string ++ fixString x


renderDocumentRecursive (DNull) _ string = do
    string ++ "null"

fixString :: String -> String 
fixString "" = "''"
fixString x = do 
    if (isNumber' x) then do
        "'" ++ x ++ "'"
    else do
        if (take 1 x == " " || last x == ' ') then
            "'" ++ x ++ "'"
        else 
            add' x



renderList :: Document -> Int -> String -> String
--renderList (DList ((DMap []):[])) c string =string ++ "- {}\n"
renderList (DList ((DMap []):xs)) c string = renderList (DList xs) c (string ++ (duplicate "  " c) ++ "- {}\n")
renderList (DList ((DMap x):xs)) c string = renderList (DList xs) c (renderMap (DMap x) (c+1) (string ++ (duplicate "  " c) ++ "- "))
renderList (DList ((DList []):xs)) c string = renderList (DList xs) c (string ++ (duplicate "  " c) ++"- []\n")
renderList (DList ((DList x):xs)) c string = renderList (DList xs) c (renderList' (DList x) (c+1) (string ++ (duplicate "  " c) ++ "- "))
renderList (DList (x:[])) c string = renderList x (c) (string ++ (duplicate "  " c) ++ "- ") ++ "\n" -- pridejom \n 
renderList (DList (x:xs)) c string = renderList (DList xs) c (renderList x c (string ++ (duplicate "  " c) ++ "- ")++ "\n" )
renderList (DMap ((x,DMap []):[])) c string = string ++  (add' x) ++ ":" ++ " {}"
renderList (DMap ((x,DMap []):xss)) c string = renderList (DMap xss) c (string ++  (add' x) ++ ": {}" ++ "\n" ++ (duplicate "  " c))
renderList (DMap ((x,DMap xs):[])) c string =  renderMap (DMap xs) (c+1) (string ++  (add' x) ++ ":" ++ "\n" ++ (duplicate "  " (c+1)))
renderList (DMap ((x,DList []):[])) c string = (string ++  (add' x) ++ ": []\n")
renderList (DMap ((x,DList xs):[])) c string =  renderMap (DList xs) (c) (string ++  (add' x) ++ ":")
renderList (DMap ((x,xs):[])) c string =  renderMap xs (c+1) (string ++  (add' x) ++ ": ") -- pridejom c+1, \n
renderList (DMap ((x,DMap xs):xss)) c string = renderList (DMap xss) c (renderMap (DMap xs) c (string ++  (add' x) ++ ":") ++ "\n" ++ (duplicate "  " c))
renderList (DMap ((x,DList []):xss)) c string = renderList (DMap xss) c (string ++  (add' x) ++ ": " ++ "[]" ++ "\n" ++ (duplicate "  " c))
renderList (DMap ((x,DList xs):xss)) c string = renderList (DMap xss) c (renderMap (DList xs) c (string ++  (add' x) ++ ":") ++ "\n" ++ (duplicate "  " c))
renderList (DMap ((x,xs):xss)) c string = renderList (DMap xss) c (renderMap xs c (string ++  (add' x) ++ ": ") ++ "\n" ++ (duplicate "  " c))
renderList (DInteger x) _ string = string ++ show x
renderList (DString x) _ string = string ++ fixString x
renderList (DNull) _ string = string ++ "null"
renderList _ _ string = string


renderList' :: Document -> Int -> String -> String
renderList' (DList ((DList []):xs)) c string = renderList (DList xs) c  (string ++ "- []\n")
renderList' (DList ((DMap []):xs)) c string = renderList (DList xs) c  (string ++ "- {}\n")
renderList' (DList ((DList x):xs)) c string = renderList (DList xs) c (renderList' (DList x) (c+1) (string ++ "- "))
renderList' (DList ((DMap ((x,(DList xss)):xsss)):xs)) c string = renderList (DList xs) c (renderMap (DMap ((x,(DList xss)):xsss)) (c+1) (string ++ "- ")++ "\n" )
renderList' (DList ((DMap ((x,xss):xsss)):xs)) c string = renderList (DList xs) c (renderMap (DMap ((x,xss):xsss)) (c+1) (string ++ "- ")++ "\n" )
renderList' (DList (x:xs)) c string = renderList (DList xs) c (renderList x c (string ++ "- ")++ "\n" )
renderList' _ _ string = string



renderMap :: Document -> Int -> String -> String
renderMap (DList ((DMap []):xs)) c string = renderMap (DList xs) c (string ++ "\n" ++ (duplicate "  " c) ++ "- " ++ "{}\n") -- ?
renderMap (DList ((DList []):xs)) c string = renderMap (DList xs) c (string ++ "\n" ++ (duplicate "  " c) ++ "- " ++ "[]\n") -- ?
renderMap (DList ((DList x):xs)) c string = renderMap (DList xs) c (renderList' (DList x) (c+1) (string ++ "\n" ++ (duplicate "  " c) ++ "- "))
renderMap (DList ((DMap x):xs)) c string = renderMap (DList xs) c (renderMap (DMap x) (c+1) (string ++ "\n" ++ (duplicate "  " c) ++ "- "))
renderMap (DList (x:xs)) c string = renderMap (DList xs) c (renderList x (c+1) (string ++ "\n" ++ (duplicate "  " c) ++ "- "))
renderMap (DMap ((x,DList []):[])) c string =  (((string ++ (add' x) ++ ": []"))++"\n")
renderMap (DMap ((x,DList []):xss)) c string =  renderMap (DMap xss) c ((string ++ (add' x) ++ ": []")++"\n" ++ (duplicate "  " c))
renderMap (DMap ((x,DList xs):[])) c string =  (renderMap (DList xs) c (string ++ (add' x) ++ ":"))++"\n" -- questionable
renderMap (DMap ((x,DList xs):xss)) c string =  renderMap (DMap xss) c ((renderMap (DList xs) c (string ++ (add' x) ++ ":"))++"\n" ++ (duplicate "  " c)) -- ??
renderMap (DMap ((x,DMap []):[])) c string = (string ++  (add' x) ++ ": {}" ++ "\n")
renderMap (DMap ((x,DMap []):xss)) c string = renderMap (DMap xss) c (string ++ (add' x) ++ ": {}" ++ "\n" ++ (duplicate "  " c))
--renderMap (DMap ((x,DMap (xs:[])):xss)) c string = renderMap (DMap xss) c ((renderMap (DMap [xs]) (c+1) (string ++  (add' x) ++ ":" ++ "\n"))++ (duplicate "  " (c))) -- -- -- bad
renderMap (DMap ((x,DMap xs):[])) c string = (renderMap (DMap xs) (c+1) (string ++  (add' x) ++ ":" ++ "\n" ++ (duplicate "  " (c+1))))-- -- --
renderMap (DMap ((x,DMap xs):xss)) c string = renderMap (DMap xss) c ((renderMap (DMap xs) (c+1) (string ++  (add' x) ++ ":" ++ "\n" ++ (duplicate "  " (c+1)))) ++ (duplicate "  " c)) -- -- --
renderMap (DMap ((x,xs):[])) c string = ((renderMap xs c (string ++ (add' x) ++ ": ")) ++ "\n") -- jei paskutinis dmapo tuplas dedam tarpa
renderMap (DMap ((x,xs):xss)) c string = renderMap (DMap xss) c (((renderMap xs c (string ++ (add' x) ++ ": "))++"\n" ++ (duplicate "  " c)))
renderMap (DInteger x) _ string = string ++ show x
renderMap (DString "") _ string = string ++ "''"
renderMap (DString x) _ string = string ++ fixString x
renderMap (DNull) _ string = string ++ "null"
renderMap _ _ string = string



add' :: String -> String 
add' x = do
    if (x == "n" || x == "N" || x == "y" || x == "Y" ) then
        "'" ++ x ++ "'"
    else x
    
isNumber' :: String -> Bool
isNumber' "" = False
isNumber' (x:xs)
    | isDigit x && xs /= "" = isNumber' xs
    | isDigit x && xs == "" =  True
    | otherwise = False

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


gameStart :: State -> Document -> Either String State
gameStart _ (DMap[]) = Left $ "No game start information!"
gameStart (State l) d
    | State l == State [("Initial_state",DNull)] = Right $ State $ ("Game", DList [DMap [("occupied_cells", DList [])], d ]) : l
    | State l /= State [("Initial_state",DNull)] = Left $ "Bad initial state!"
gameStart _ _ = Left $ "Something went wrong while starting the game!"



hint :: State -> Document -> Either String State

hint _ (DMap[(_,DNull)]) = Left "Empty hints!"
hint _ (DMap[(string,_)]) 
    | string /= "coords" = Left "Wrong DMap!"
hint (State[]) _ = Left "Empty state!"
hint (State (l:ls)) t = Right $ State (hintFunc1 l t : ls)

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
