{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger, DNull), Check(..), Coord(..) )
import Lib1 (State(..))


-- IMPLEMENT
-- First, make Check an instance of ToDocument class
-- checkas yra coords kurias reikia paverst į document
-- o visa kita yra jau document, tai siusim tiesiai į render

-- instance ToDocument Check where
--     toDocument (Check a) = check' a coordMap

-- check' :: [Coord] -> Document -> Document
-- check' ((Coord x y):xs) cordMap
--     | xs /= [] = do
--         [DMap[("Col", DInteger x),("Row", DInteger y)]]:cordMap
--         check' xs cordMap
--     | xs == [] = [DMap[("Col", DInteger x),("Row", DInteger y)]]:cordMap


-- coordMap :: DList[DMap[(String,DInteger),(String,DInteger)]]
-- coordMap = DList[]


instance ToDocument Check where
    toDocument (Check x) = DList (check' x [])


check' :: [Coord] -> [Document] ->  [Document]
check' (x:xs) dab
    | xs /= [] = check' xs ((coordGet x) : dab)
    | xs == [] = coordGet x : dab
check' _ _ = []
    
coordGet :: Coord -> Document
coordGet (Coord x y) = DMap[("col",DInteger x),("row",DInteger y)]

-- KODEL VEIKIA CHECK SU TOGGLE, BET NEVEIKIA, KAI SU HINT!??????


-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument x = error (show "==== " ++ show x ++ "====")

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
--gameStart (State l) d = Right $ State $ ("Game started: " ++ show d, DNull) : l
gameStart (State l) d = Right $ State $ ("Game", DList [DMap [("occupied_cells", DList [])], d ]) : l
gameStart _ _ = Left $ "Something went wrong while starting the game!"


-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State l) h = Right $ State $ ("Hint " ++ show h, DNull) : l
hint _ _ = Left $ "Something went wrong with hints!"
--hint (State (l:ls)) t = Right $ State (hintFunc1 l t : ls)


--Return new tuple for state with hint coordinate values added to "occupied_cells"
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

