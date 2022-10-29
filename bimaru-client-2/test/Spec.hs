import Test.Tasty
import Test.Tasty.HUnit
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))
import Distribution.Types.DependencyMap (fromDepMap)
import Data.String (String)
import Text.Read (Lexeme(String))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "string" $
        renderDocument (String "Test") @?= "Test"
    , testCase "dmap" $
        renderDocument (DMap[("first", DInteger 5)]) @?= dMap
    , testCase "list of ints in a list" $
        renderDocument (DList [DList [DInteger 5, DInteger 6]]) @?= listInList
    , testCase "List of Lists" $
        renderDocument (DList [DMap[("first",DList [DInteger 1, DInteger 2])], DInteger 1, DInteger 2, DInteger 3]) @?= listOfLists   
    , testCase "list In list In List" $
        renderDocument (DList [DList [DList [DInteger 5, DInteger 6],DInteger 3, DInteger 4],DInteger 1, DInteger 2]) @?= listInListInList
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]
listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]
dMap :: String
dMap = unlines[
      "---"
    , "first: 5"
  ]
listInList :: String
listInList = unlines[
      "---"
     ,"- - 5"
     ,"  - 6"
  ]
listOfLists :: String
listOfLists = unlines[
      "---"
     ,"- first"
     ,"- - 1"
     ,"  - 2"
     ,"- 1"
     ,"- 2"
     ,"- 3"
  ]
listInListInList :: String
listInListInList = unlines[
      "---"
     ,"- - - 5"
     ,"    - 6"
     ,"  -  3"
     ,"  -  4"
     ,"- 1"
     ,"- 2"
  ]
gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []