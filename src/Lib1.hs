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
--toggle (State l) t = State $ ("Toggle " ++ show t, DNull) : l
toggle (State (l1,[[(l2,ls)]])) (x:y:[]) = 
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
hint (State l) h = State $ ("Hint ", h) : l