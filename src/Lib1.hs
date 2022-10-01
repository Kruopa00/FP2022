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


mapas :: [((Int, Int),Int)]
mapas = [( (1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),((1,8),0),((1,9),0),((1,10),0),
         ((2,1),0),((2,2),0),((2,3),0),((2,4),0),((2,5),0),((2,6),0),((2,7),0),((2,8),0),((2,9),0),((2,10),0),
         ((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((3,6),0),((3,7),0),((3,8),0),((3,9),0),((3,10),0),
         ((4,1),0),((4,2),0),((4,3),0),((4,4),0),((4,5),0),((4,6),0),((4,7),0),((4,8),0),((4,9),0),((4,10),0),
         ((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),0),((5,6),0),((5,7),0),((5,8),0),((5,9),0),((5,10),0),
         ((6,1),0),((6,2),0),((6,3),0),((6,4),0),((6,5),0),((6,6),0),((6,7),0),((6,8),0),((6,9),0),((6,10),0),
         ((7,1),0),((7,2),0),((7,3),0),((7,4),0),((7,5),0),((7,6),0),((7,7),0),((7,8),0),((7,9),0),((7,10),0),
         ((8,1),0),((8,2),0),((8,3),0),((8,4),0),((8,5),0),((8,6),0),((8,7),0),((8,8),0),((8,9),0),((8,10),0),
         ((9,1),0),((9,2),0),((9,3),0),((9,4),0),((9,5),0),((9,6),0),((9,7),0),((9,8),0),((9,9),0),((9,10),0),
         ((10,1),0),((10,2),0),((10,3),0),((10,4),0),((10,5),0),((10,6),0),((10,7),0),((10,8),0),((10,9),0),((10,10),0)]

array :: [a]
array = []

map1 :: [((Int, Int), Int)] -> [Int] -> [Int]
map1 (((_,y), z) : xs) array
        | y == 10 = ( z : array) ++ [10] ++ map1 xs array
        | otherwise = ( z : array) ++ map1 xs array
map1 _ _= []

-- | x == 10 && y == 10 = ( z : array) ++ map1 xs array

-- IMPLEMENT
-- renders your game board
render :: State -> String
render a = puttingCol (take 11 (deeper a resultArray)) ++ foo (map1 mapas array) newArray (drop 9 (deeper a resultArray))
--render a = show $ deeper a resultArray
--render a = foo $ map1 mapas array
--maps[((x,y),DInt)] -- 100 tuplu -> 100 reiksmiu

newArray :: String
newArray = ""

puttingCol :: [Int] -> String
puttingCol (x:xs) = (show x ++ " ") ++ puttingCol xs
puttingCol (x:_) = "\n" ++ show x ++ " - "
puttingCol _ = "\n"



foo :: [Int] -> String -> [Int] -> String
foo (x:xs) newArray (y:ys)
    | x /= 10 = newArray ++ show x ++ " " ++ foo xs newArray (y:ys)
    | otherwise = newArray ++ "\n" ++ show y ++ " - " ++ foo xs newArray ys
foo _ _ _ = ""


convert :: Document -> Int
convert (DInteger x) = x


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



resultArray :: [Int]
resultArray = []

deeper :: State -> [Int] -> [Int]
deeper (State (l:ls)) resultArray = deeper1 l resultArray

deeper1 :: (String, Document) -> [Int] -> [Int]
deeper1 (l, ls) resultArray = deeper2 ls resultArray

deeper2 :: Document -> [Int] -> [Int]
deeper2 (DList [l,ls]) resultArray = deeper3 ls resultArray

deeper3 :: Document -> [Int] -> [Int]
deeper3 (DMap (l:ls)) resultArray = deeper4 ls resultArray

deeper4 :: [(String, Document)] -> [Int] -> [Int]
deeper4 (l:ls) resultArray = cols l resultArray ++ deeper5 ls resultArray

deeper5 :: [(String, Document)] -> [Int] -> [Int]
deeper5 (l:ls) resultArray = cols l resultArray

cols :: (String, Document) -> [Int] -> [Int]
cols (l,ls) resultArray = cols1 ls resultArray

cols1 :: Document -> [Int] -> [Int]
cols1 (DList (DInteger x : ls)) resultArray = cols1 (DList ls) $ x : resultArray
cols1 _ resultArray = reverse resultArray

{-cols2 :: Document -> Int
cols2 (DInteger x) = x
cols2 _ = -1-}
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


