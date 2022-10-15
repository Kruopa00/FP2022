{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (DMap, DList, DInteger), Check )
import Lib1 (State(..))
--import Distribution.Compat.DList (DList)

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
-- checkas yra coords kurias reikia paverst į document
-- o visa kita yra jau document, tai siusim tiesiai į render
instance ToDocument Check where
    toDocument a = DInteger 5

-- check' :: [Coord] -> DList[] -> Document
-- check' ((Coord x y):xs) cordMap
--     | xs \= [] = do
--         [DMap[("Col", DInteger x),("Row", DInteger y)]]:cordMap
--         check' xs cordMap
--     | xs == [] = [DMap[("Col", DInteger x),("Row", DInteger y)]]:cordMap

-- cordMap :: DList[DMap[(String, DInteger),(String, DInteger)]]
-- cordMap = DList[]

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument _ = error "Implement me"

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State l) d = Right $ State $ ("Game started: " ++ show d) : l

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
