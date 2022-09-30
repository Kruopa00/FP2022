{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint,
) where

import Types


-- This is a stateS of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State [(String, Document)]
    deriving Show


-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [("Initial state", DNull)]


-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State l) d = State $ ("Game", DList [DMap [("occupied_cells", DList []), ("hints", DList [])], d ]) : l



-- IMPLEMENT
-- renders your game board
render :: State -> String
render a = show a


-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []


-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State (l:ls)) t = State ((func1 l t) : ls)

func1 :: (String, Document) -> [String] -> (String, Document)
func1 (l,ls) t = (l, func2 ls t)

func2 :: Document -> [String] -> Document
func2 (DList (l:ls)) t = DList ((func3 l t):ls)

func3 :: Document -> [String] -> Document
func3 (DMap (l:ls)) t = DMap (func4 l t :ls)

func4 :: (String, Document) -> [String] -> (String, Document)
func4 (l, ls) t = (l, func5 ls t)

func5 :: Document -> [String] -> Document
func5 (DList l) t = (DList (func6 l t))

func6 :: [Document] -> [String] -> [Document]
func6 l (x:y:[]) = DMap ([("col", DInteger (read x)), ("row",DInteger (read y))]):l







    --if(l == "occupied_cells")
        --thenW
            --DMap ([("col", DInteger (read x)), ("row",DInteger (read y))]) : ls
        --else
            --toggle ("test":ls) (x:y:[]) 

--gameStart (State l) d = State $ ("Game", DList [DMap [("occupied_cells", DList []), ("hints", DList [])], d ]) : l


    --State $ ("Toggle", DList [DMap ([("col", DInteger (read x)), ("row",DInteger (read y))])]) : l
    --1 dmap (toggle, dlist[dint, dint])
    --2 (x:xs) s=="toggle", dlist : naujas 
    --3


-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State (l:ls)) t = State ((hintFunc1 l t) : ls)

hintFunc1 :: (String, Document) -> Document -> (String, Document)
hintFunc1 (l,ls) t = (l, hintFunc2 ls t)

hintFunc2 :: Document -> Document -> Document
hintFunc2 (DList (l:ls)) t = DList ((hintFunc3 l t):ls)

hintFunc3 :: Document -> Document -> Document
hintFunc3 (DMap (l:ls)) t = DMap (hintFunc4 l t :ls)

hintFunc4 :: (String, Document) -> Document -> (String, Document)
hintFunc4 (l, ls) t = (l, hintFunc5 ls t)

hintFunc5 :: Document -> Document -> Document
hintFunc5 (DList l) t = (DList (hintFunc6 l t))

hintFunc6 :: [Document] -> Document -> [Document]
hintFunc6 l t = (hintFunc7 t):l

hintFunc7 :: Document -> Document
hintFunc7 (DMap [l]) = hintFunc8 l

hintFunc8 :: (String, Document) -> Document
hintFunc8 (l,ls) =  hintFunc9 ls

hintFunc9 :: Document -> Document
hintFunc9 (DMap [l,(ls,DNull)]) = hintFunc10 l
hintFunc9 (DMap [l,(ls,lss)]) = hintFunc8 (ls,lss)

hintFunc10 :: (String, Document) -> Document
hintFunc10 (l,ls) = ls


