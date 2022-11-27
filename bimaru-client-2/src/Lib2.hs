{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart, renderDocumentRecursive) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DString, DNull), Check(..), Coord(..) )
import Lib1 (State(..))


instance ToDocument Check where
    toDocument (Check x) = DMap [("coords",DList (check' x []))]


check' :: [Coord] -> [Document] ->  [Document]
check' (x:xs) dab
    | xs /= [] = check' xs ((coordGet x) : dab)
    | xs == [] = coordGet x : dab
check' _ _ = []

coordGet :: Coord -> Document
coordGet (Coord x y) = DMap[("col",DInteger x),("row",DInteger y)]



renderDocument :: Document -> String
renderDocument x = "---\n" ++ renderDocumentRecursive x 0 ""

renderDocumentRecursive :: Document -> Int -> String -> String

renderDocumentRecursive (DMap x) c string = do
    renderMap (DMap x) c string

renderDocumentRecursive (DList x) c string = do
    renderList (DList x) c (string)

renderDocumentRecursive (DInteger x) _ string = do
    string ++ show x

renderDocumentRecursive (DString x) _ string = do
    string ++ x

renderDocumentRecursive (DNull) _ string = do
    string ++ "null"



renderList :: Document -> Int -> String -> String
renderList (DList ((DList x):xs)) c string = renderList (DList xs) c (renderList' (DList x) (c+1) (string ++ "- "))
renderList (DList (x:[])) c string = renderList x (c) (string ++ (duplicate "  " c) ++ "- ") ++ "\n" -- pridejom \n 
renderList (DList (x:xs)) c string = renderList (DList xs) c (renderList x c (string ++ (duplicate "  " c) ++ "- ")++ "\n" )
renderList (DMap ((x,xs):[])) c string =  renderMap xs (c+1) (string ++  x ++ ": ") -- pridejom c+1, \n
renderList (DMap ((x,xs):xss)) c string = renderList (DMap xss) c (renderMap xs c (string ++  x ++ ": ") ++ "\n" ++ (duplicate "  " c))
renderList (DInteger x) _ string = string ++ show x
renderList (DString x) _ string = string ++ x
renderList (DNull) _ string = string ++ "null"
renderList _ _ string = string


renderList' :: Document -> Int -> String -> String
renderList' (DList ((DList x):xs)) c string = renderList (DList xs) c (renderList' (DList x) (c+1) (string ++ "- "))
renderList' (DList (x:xs)) c string = renderList (DList xs) c (renderList x c (string ++ "- ")++ "\n" )
renderList' _ _ string = string



renderMap :: Document -> Int -> String -> String
renderMap (DList ((DList x):xs)) c string = renderMap (DList xs) c (renderList' (DList x) (c+1) (string ++ "\n" ++ (duplicate "  " c) ++ "- "))
renderMap (DList (x:xs)) c string = renderMap (DList xs) c (renderList x (c+1) (string ++ "\n" ++ (duplicate "  " c) ++ "- "))
renderMap (DMap ((x,DList xs):[])) c string =  (renderMap (DList xs) c (string ++ x ++ ": "))++"\n"
renderMap (DMap ((x,DMap xs):xss)) c string = renderMap (DMap xss) c (renderMap (DMap xs) (c+1) (string ++  x ++ ": " ++ "\n" ++ (duplicate "  " (c+1))))
renderMap (DMap ((x,xs):[])) c string = renderMap xs c (string ++ x ++ ": ") -- jei paskutinis dmapo tuplas dedam tarpa
renderMap (DMap ((x,xs):xss)) c string = renderMap (DMap xss) c (renderMap xs c (string ++ x ++ ": "))
renderMap (DInteger x) _ string = string ++ show x
renderMap (DString x) _ string = string ++ x
renderMap (DNull) _ string = string ++ "null"
renderMap _ _ string = string






duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


gameStart :: State -> Document -> Either String State
gameStart _ (DMap[]) = Left $ "No game start information!"
gameStart (State l) d
    | State l == State [("Initial_state",DNull)] = Right $ State $ ("Game", DList [DMap [("occupied_cells", DList [])], d ]) : l
    | State l /= State [("Initial_state",DNull)] = Left $ "Bad Initial_state!"
gameStart _ _ = Left $ "Something went wrong while starting the game!"



hint :: State -> Document -> Either String State
hint x y = error $ show y
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
