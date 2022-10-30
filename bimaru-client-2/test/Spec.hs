import Test.Tasty
import Test.Tasty.HUnit
import Lib2 (renderDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $ -- ok
        renderDocument DNull @?= "---\nnull"
    , testCase "int" $ -- ok
        renderDocument (DInteger 5) @?= "---\n5"
    , testCase "string" $ -- ok
        renderDocument (DString "Test") @?= "---\nTest"
    , testCase "list of ints" $ 
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "dmap" $
        renderDocument (DMap[("first", DInteger 5)]) @?= dMap
    , testCase "list of ints in a list" $
        renderDocument (DList [DList [DInteger 5, DInteger 6]]) @?= listInList
    , testCase "List of Lists" $
        renderDocument (DList [DMap[("first",DList [DInteger 1, DInteger 2])], DString "test", DInteger 2, DInteger 3]) @?= listOfLists   
    , testCase "list In list In List" $
        renderDocument (DList [DList [DList [DInteger 5, DInteger 6],DInteger 3, DInteger 4],DInteger 1, DInteger 2]) @?= listInListInList
    , testCase "checkFormat" $
        renderDocument (DMap[("Coords", DList[DMap[("col",DInteger 1),("row",DInteger 2)],DMap[("col",DInteger 3),("row",DInteger 4)],DMap[("col",DInteger 5),("row",DInteger 6)]])]) @?= checkTest
    , testCase "dmap in dmap" $
        renderDocument (DMap[("first", DMap[("third", DList[DInteger 1])]),("second", DMap[("forth", DList[DInteger 2])])]) @?= dmapInDmap 
    , testCase "dlist in dmap" $ 
        renderDocument (DMap[("first", DList[DMap[("second", DList [DInteger 1, DInteger 2])]])]) @?= dlistInDmap    
    , testCase "dlists in dlist" $
        renderDocument (DList[DList[DList[DMap[("first", DList[DInteger 1, DString "test"])]]], DInteger 3, DNull]) @?= dlistsInList  


    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]
listOfInts :: String
listOfInts = unlines [
      "---",
      "- 5",
      "- 6"
  ]
dMap :: String
dMap = "---\nfirst: 5"

listInList :: String
listInList = unlines[
      "---"
     ,"- - 5"
     ,"  - 6"
  ]
listOfLists :: String
listOfLists = unlines[
      "---"
     ,"- first: "
     ,"  - 1"
     ,"  - 2"
     ,"- test"
     ,"- 2"
     ,"- 3"
  ]
listInListInList :: String
listInListInList = unlines[
      "---"
     ,"- - - 5"
     ,"    - 6"
     ,"  - 3"
     ,"  - 4"
     ,"- 1"
     ,"- 2"
  ]
dmapInDmap :: String
dmapInDmap = unlines[
      "---",
      "first: ",
      "  third: ",
      "  - 1",
      "second: ",
      "  forth: ",
      "  - 2"
  ]
dlistInDmap :: String
dlistInDmap = unlines[
      "---",
      "first: ",
      "- second: ",
      "    - 1",
      "    - 2"
  ]
dlistsInList :: String
dlistsInList = unlines[
      "---",
      "- - - first: ",
      "      - 1",
      "      - test",
      "- 3",
      "- null"
  ]

checkTest :: String
checkTest = unlines [
      "---",
      "Coords: ",
      "- col: 1",
      "  row: 2",
      "- col: 3",
      "  row: 4",
      "- col: 5",
      "  row: 6"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []