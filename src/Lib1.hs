{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint,
) where

import Types

--GAME INFO
--
--Coordinates are from 1 to 10 and start from top left corner
--
--toggle 
--Function takes two Int arguments with values from 1 to 10 (If not - game crashes). It toggles the coordinates value in the map.
--
--hint
--Function takes one Int argument (If not - game crashes). It automatically toggles the coordinates. 
--The number indicates which hint you will get. Ex. hint 1 - will give you first hint, hint 7 - will give you 7th hint, but not 7 hints.
--
--show 
--Function doesnt take arguments, it simply renders the map to the screen.
--
--check
--Function returns if you solved the puzzle. There are two ways to solve it, but check only recognizes one of the ways to solve it. 
--
--Made by Rokas Čebatorius, Arnas Klimašauskas, Vytautas Krupavičius, Ugnius Motiejunas



-- This is a stateS of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State [(String, Document)]
    deriving Show

-- Game map
-- Format: ((col, row), value)
-- Values: 1 - occupied, 0 - free
gameMap :: [((Int, Int),Int)]
gameMap = [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((0,4),0),((0,5),0),((0,6),0),((0,7),0),((0,8),0),((0,9),0),
         ((1,0),0),((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),((1,8),0),((1,9),0),
         ((2,0),0),((2,1),0),((2,2),0),((2,3),0),((2,4),0),((2,5),0),((2,6),0),((2,7),0),((2,8),0),((2,9),0),
         ((3,0),0),((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((3,6),0),((3,7),0),((3,8),0),((3,9),0),
         ((4,0),0),((4,1),0),((4,2),0),((4,3),0),((4,4),0),((4,5),0),((4,6),0),((4,7),0),((4,8),0),((4,9),0),
         ((5,0),0),((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),0),((5,6),0),((5,7),0),((5,8),0),((5,9),0),
         ((6,0),0),((6,1),0),((6,2),0),((6,3),0),((6,4),0),((6,5),0),((6,6),0),((6,7),0),((6,8),0),((6,9),0),
         ((7,0),0),((7,1),0),((7,2),0),((7,3),0),((7,4),0),((7,5),0),((7,6),0),((7,7),0),((7,8),0),((7,9),0),
         ((8,0),0),((8,1),0),((8,2),0),((8,3),0),((8,4),0),((8,5),0),((8,6),0),((8,7),0),((8,8),0),((8,9),0),
         ((9,0),0),((9,1),0),((9,2),0),((9,3),0),((9,4),0),((9,5),0),((9,6),0),((9,7),0),((9,8),0),((9,9),0)]
         

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [("Initial state", DNull)]


-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State l) d = State $ ("Game", DList [DMap [("occupied_cells", DList [])], d ]) : l


-- IMPLEMENT
-- renders your game board
render :: State -> String
render a = puttingCol (take 10 (getResult a []))  ++ puttingValues (map1 (updateMap gameMap [] (getToggles a [])) []) "" (drop 10 (getResult a []))


-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State (l:ls)) t = State ((toggleFunc1 l t) : ls)


-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State (l:ls)) t = State ((hintFunc1 l t) : ls)


-- IMPLEMENT
-- Make check from current state
-- Only works with one answer 
mkCheck :: State -> Check
mkCheck a = (getCheck (updateMap gameMap [] (getToggles a [])) (Check []))



--Takes map values and puts into list
map1 :: [((Int, Int), Int)] -> [Int] -> [Int]
map1 (((_,y), z) : xs) array
        | y == 9 = ( z : array) ++ [10] ++ map1 xs array
        | otherwise = ( z : array) ++ map1 xs array
map1 _ _= []


--Create new map with updated values from toggled list
updateMap :: [((Int, Int),Int)] -> [((Int, Int),Int)] -> [(Int, Int)] -> [((Int, Int),Int)]
updateMap  (x:xs) newMap toggles
    |xs /= [] = do
        (updateMap  xs (((mapFunc x toggles)):newMap) toggles)
    |xs == [] = reverse ((mapFunc x toggles):newMap)
    

--Changing values
mapFunc :: ((Int,Int), Int) -> [(Int,Int)] -> ((Int,Int), Int)
mapFunc ((x,y),s) ((cx,cy):cxs) = if ((x == cy) && (y == cx)) then mapFunc ((x, y), if s == 0 then 1 else 0) cxs else mapFunc ((x,y),s) cxs
mapFunc ((x,y),s) _ = ((x,y),s)
    

--Map string fromatting
puttingCol :: [Int] -> String
puttingCol (x:xs) = (show x ++ " ") ++ puttingCol xs
puttingCol _ = "\n____________________\n"


--Map values to string and formatting
puttingValues :: [Int] -> String -> [Int] -> String
puttingValues (x:xs) newArray (y:ys)
    | x /= 10 = newArray ++ show x ++ " " ++ puttingValues xs newArray (y:ys)
    | otherwise = newArray ++  "| " ++ show y ++ "\n" ++ puttingValues xs newArray ys
puttingValues _ _ _ = ""



--Gets toggled coordinate values from state and puts them into list 
getToggles :: State -> [(Int, Int)] -> [(Int, Int)]
getToggles (State (l:ls)) cords = getTogglesFunc1 l cords

getTogglesFunc1 :: (String, Document)  -> [(Int, Int)] -> [(Int, Int)]
getTogglesFunc1 (l,ls) cords = getTogglesFunc2 ls cords

getTogglesFunc2 :: Document  -> [(Int, Int)] -> [(Int, Int)]
getTogglesFunc2 (DList (l:_)) cords = getTogglesFunc3 l cords

getTogglesFunc3 :: Document  -> [(Int, Int)] -> [(Int, Int)]
getTogglesFunc3 (DMap (l:_)) cords = getTogglesFunc4 l cords

getTogglesFunc4 :: (String, Document)  -> [(Int,  Int)] -> [(Int, Int)]
getTogglesFunc4 (_,DList []) cords = cords
getTogglesFunc4 (_,ls) cords = getTogglesFunc5 ls cords

getTogglesFunc5 :: Document  -> [(Int, Int)] -> [(Int, Int)]
getTogglesFunc5 (DList (x:xs)) cords
    | xs /= [] = do
         getTogglesFunc5  (DList xs) (getTogglesFunc6 x cords)
    | xs == [] = getTogglesFunc6 x cords

getTogglesFunc6 :: Document  -> [(Int, Int)] -> [(Int, Int)]
getTogglesFunc6 (DMap [(_,DInteger x),(_,DInteger y)]) cords = do
    (x,y):cords
getTogglesFunc6 _ cords = (-1,-1):cords



--Create Check from map
getCheck:: [((Int, Int),Int)] -> Check -> Check
getCheck (x:xs) (Check c) 
    | xs /= [] = do
        getCheck xs (Check (getCheckFunc1 x c))
    | xs == [] = Check (getCheckFunc1 x c)
getCheck _ (Check c) = (Check c) 

getCheckFunc1 :: ((Int, Int),Int)  -> [Coord] -> [Coord]
getCheckFunc1 ((x,y),s) c = if s == 1 then Coord y x: c else c
getCheckFunc1 _ c = c



--Returns new tuple for State with toggled coordinate values in "occupied_cells"
toggleFunc1 :: (String, Document) -> [String] -> (String, Document)
toggleFunc1 (l,ls) t = (l, toggleFunc2 ls t)

toggleFunc2 :: Document -> [String] -> Document
toggleFunc2 (DList (l:ls)) t = DList ((toggleFunc3 l t):ls)

toggleFunc3 :: Document -> [String] -> Document
toggleFunc3 (DMap (l:ls)) t = DMap (toggleFunc4 l t :ls)

toggleFunc4 :: (String, Document) -> [String] -> (String, Document)
toggleFunc4 (l, ls) t = (l, toggleFunc5 ls t)

toggleFunc5 :: Document -> [String] -> Document
toggleFunc5 (DList l) t = (DList (toggleFunc6 l t))

toggleFunc6 :: [Document] -> [String] -> [Document]
toggleFunc6 l (x:y:[]) = DMap ([("col", DInteger (read x-1)), ("row",DInteger (read y-1))]):l



--Get given number of ship in a col and row
getResult :: State -> [Int] -> [Int]
getResult (State (l:ls)) resultArray = getResultFunc1 l resultArray

getResultFunc1 :: (String, Document) -> [Int] -> [Int]
getResultFunc1 (l, ls) resultArray = getResultFunc2 ls resultArray

getResultFunc2 :: Document -> [Int] -> [Int]
getResultFunc2 (DList [l,ls]) resultArray = getResultFunc3 ls resultArray

getResultFunc3 :: Document -> [Int] -> [Int]
getResultFunc3 (DMap (l:ls)) resultArray = getResultFunc4 ls resultArray

getResultFunc4 :: [(String, Document)] -> [Int] -> [Int]
getResultFunc4 (l:ls) resultArray = cols l resultArray ++ getResultFunc5 ls resultArray

getResultFunc5 :: [(String, Document)] -> [Int] -> [Int]
getResultFunc5 (l:ls) resultArray = cols l resultArray

cols :: (String, Document) -> [Int] -> [Int]
cols (l,ls) resultArray = cols1 ls resultArray

cols1 :: Document -> [Int] -> [Int]
cols1 (DList (DInteger x : ls)) resultArray = cols1 (DList ls) $ x : resultArray
cols1 _ resultArray = reverse resultArray



--Return new tuple for state with hint coordinate values added to "occupied_cells"
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
