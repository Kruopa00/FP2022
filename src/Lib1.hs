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
hint (State l) h = State $ ("Hint " ++ show h, DNull) : l