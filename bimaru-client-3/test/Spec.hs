import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib2 (renderDocument)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]




friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    , testCase "empty DList" $
        parseDocument "[]" @?= Right (DList [])
    , testCase "empty DMap" $
        parseDocument "{}" @?= Right (DMap [])
    , testCase "empty DString" $
        parseDocument "" @?= Right (DString "")
    , testCase "DString one space" $
        parseDocument " " @?= Right (DString " ")
    , testCase "integer" $
        parseDocument "0" @?= Right (DInteger 0)
    , testCase "-integer" $
        parseDocument "-1" @?= Right (DInteger (-1))    
    , testCase "string" $
        parseDocument "hello" @?= Right (DString "hello")
    , testCase "empty DList" $
        parseDocument "[]" @?= Right (DList [])
    , testCase "empty DMap" $
        parseDocument "{}" @?= Right (DMap [])
    , testCase "Dlist with -int" $
        parseDocument "- -1" @?= Right (DList [DInteger (-1)])
    , testCase "number as string" $
        parseDocument "'1'\n" @?= Right (DString "1")
    , testCase "number as string in list" $
        parseDocument "- '1'\n- labas\n- -3\n- '-2'" @?= Right (DList[DString "1", DString "labas", DInteger (-3), DString "-2"])
    , testCase "empty" $
        parseDocument "" @?= Right (DString "")
    , testCase "DList with all dlist items" $
        parseDocument  "- 9\n- null\n- veikia\n- - 36\n- Mapas: null"  @?= Right (DList [DInteger 9, DNull, DString "veikia", DList[DInteger 36],DMap[("Mapas", DNull)]])
    , testCase "DList in Dlists" $
        parseDocument "- - - 5\n    - 6\n  - 3\n  - 4\n- 1\n- 2" @?= Right (DList [DList [DList [DInteger 5, DInteger 6],DInteger 3, DInteger 4],DInteger 1, DInteger 2])
    , testCase "Dmap in Dmaps" $
        parseDocument "first:\n  third:\n  - 1\nsecond:\n  forth:\n  - 2" @?= Right (DMap[("first", DMap[("third", DList[DInteger 1])]),("second", DMap[("forth", DList[DInteger 2])])])
        , testCase "test" $
        parseDocument "- Z:\n  - 0\n  - []\n-  1" @?= Right (DList [DMap [("Z",DList [DInteger 0,DList []])],DString " 1"])
    , testCase "test1" $
        parseDocument "- {}\n- 2" @?= Right (DList[DMap [], DInteger 2])
    , testCase "test2" $
        parseDocument "e8 " @?= Right (DString "e8 ")
    , testCase "test3" $
        parseDocument "e:\n- - 2\n- - yah: '2'\n  - 0\n- -1\nSS: {}\n" @?= Right (DMap [("SS",DMap []),("e",DList [DList [DInteger 2],DList [DMap [("yah",DString "2")],DInteger 0],DInteger (-1)])])
    , testCase "test4" $
        parseDocument "- FqskDhayA: 1\n- ' '\n- - lAa: ' uJ1J1 F9'\n    Iev: []\n" @?= Right (DList [DMap [("FqskDhayA",DInteger 1)],DString " ",DList [DMap [("Iev",DList []),("lAa",DString " uJ1J1 F9")]]])
    , testCase "test5" $
        parseDocument "- -2\n- -4\n- ''\n- []\n" @?= Right (DList [DInteger (-2),DInteger (-4),DString "",DList []])
    , testCase "test6" $
        parseDocument "Kv: ' '\naZifp:\n- F8 9T\n- - -5\n  - ''\n- - 5\n  - -3\n" @?= Right (DMap [("Kv",DString " "),("aZifp",DList [DString "F8 9T",DList [DInteger (-5),DString ""],DList [DInteger 5,DInteger (-3)]])])
    , testCase "test7" $
        parseDocument "Z: ''\n'N':\n- -1\n- -1\neucu: []\n" @?= Right (DMap [("N",DList [DInteger (-1),DInteger (-1)]),("Z",DString ""),("eucu",DList [])])
    , testCase "test8" $
        parseDocument "- ' '\n- ' '\n" @?= Right (DList [DString " ",DString " "])
    , testCase "test9" $
        parseDocument "'N': '7'\n" @?= Right (DMap [("N",DString "7")])    
    , testCase "test10" $
        parseDocument "z:\n  hW:\n  - 0\n  - - Cz: -1\n      JR: []\n      r: '27'\n  - 0\n  DEe: 3B\n" @?= Right (DMap [("z",DMap [("DEe",DString "3B"),("hW",DList [DInteger 0,DList [DMap [("Cz",DInteger (-1)),("JR",DList []),("r",DString "27")]],DInteger 0])])])
    , testCase "test11" $
        parseDocument "- - - -3\n    - 3\n  - JG: 0\n    qY:\n    - ' p'\n    - R:\n        B: H\n        JU:\n        - 0\n        - '64'\n        - []\n  - {}\n- -3\n" @?= Right (DList [DList [DList [DInteger (-3),DInteger 3],DMap [("JG",DInteger 0),("qY",DList [DString " p",DMap [("R",DMap [("B",DString "H"),("JU",DList [DInteger 0,DString "64",DList []])])]])],DMap []],DInteger (-3)])
    , testCase "test12" $
        parseDocument "- qi:\n  - VcAC: ''\n  xl: 2\n" @?= Right (DList [DMap [("qi",DList [DMap [("VcAC",DString "")]]),("xl",DInteger 2)]])
    , testCase "test13" $
        parseDocument "QLs:\n  A:\n  - 'm  '\n  - -1\n  - l: []\n    o: -1\n    ap:\n      DWf: -3\n      'Y': []\nBM: []\ni: j0L\n" @?= Right (DMap [("BM",DList []),("QLs",DMap [("A",DList [DString "m  ",DInteger (-1),DMap [("ap",DMap [("DWf",DInteger (-3)),("Y",DList [])]),("l",DList []),("o",DInteger (-1))]])]),("i",DString "j0L")])
    , testCase "test14" $
        parseDocument "---\n- []\n- 'n': []\n  g:\n    LR: -3\n" @?= Right (DList [DList [],DMap [("g",DMap [("LR",DInteger (-3))]),("n",DList [])]])
    , testCase "test15" $
        parseDocument "---\n- BjA: 2\n  PL: '4  '\n- 3\n- ''\n"  @?= Right (DList [DMap [("BjA",DInteger 2),("PL",DString "4  ")],DInteger 3,DString ""])
    , testCase "test16" $
        parseDocument "---\n'N':\n- NffY: -1\n- - -1\n- 6\n" @?=  Right (DMap [("N",DList [DMap [("NffY",DInteger (-1))],DList [DInteger (-1)],DInteger 6])])
    , testCase "test17" $
        parseDocument "hy: []\nU:\n- UC:\n  - J\n  Li: []\n- ''\n" @?= Right (DMap [("U",DList [DMap [("Li",DList []),("UC",DList [DString "J"])],DString ""]),("hy",DList [])])  
    
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

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
    , testCase "empty dmap" $
        renderDocument (DMap[("first", DMap [])]) @?= "---\nfirst: {}\n"
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
    , testCase "dlists in dlist" $
        renderDocument (DMap [("MV",DString " m"),("eA",DList [DString "X"])]) @?= "---\nMV: ' m'\neA:\n- X\n"
    , testCase "test" $
        renderDocument (DMap [("m",DList []),("u",DString "5I")]) @?= "---\nm: []\nu: 5I\n"
    , testCase "test2" $
        renderDocument (DMap [("Z",DMap [("BI",DInteger 2),("BS",DInteger (-1))]),("f",DInteger (-2))]) @?= "---\nZ:\n  BI: 2\n  BS: -1\nf: -2\n"
    , testCase "test3" $
        renderDocument (DList [DInteger (-3),DInteger (-1),DMap [("n",DMap []),("UFR",DString "R "),("bvg",DInteger (-1))]]) @?= "---\n- -3\n- -1\n- 'n': {}\n  UFR: 'R '\n  bvg: -1\n"
    , testCase "test4" $
        renderDocument (DList [DList [],DList [DInteger 0,DList [DString "",DString "Y"]]]) @?= "---\n- []\n- - 0\n  - - ''\n    - 'Y'\n"
    , testCase "test5" $
        renderDocument (DMap [("v",DList [DMap [("P",DMap [("t",DString "q ")])]])]) @?= "---\nv:\n- P:\n    t: 'q '\n"
    , testCase "test6" $
        renderDocument (DList [DMap [("XAop",DString "8oh ")],DList [DList []],DList [DMap [("GwITlZZs",DMap [])],DInteger 0],DString "   v0K"]) @?= "---\n- XAop: '8oh '\n- - []\n- - GwITlZZs: {}\n  - 0\n- '   v0K'\n"
    , testCase "test7" $
        renderDocument (DList [DList [DMap [],DInteger 0,DList []],DInteger 3]) @?= "---\n- - {}\n  - 0\n  - []\n- 3\n"
    , testCase "test8" $
        renderDocument (DList [DMap [("WiYI",DList [DInteger 1]),("zYBs",DString "jf"),("a",DInteger 4)]]) @?= "---\n- WiYI:\n  - 1\n  zYBs: jf\n  a: 4\n"
    , testCase "test9" $
        renderDocument (DMap [("mPztkEPS",DMap []),("sbn",DMap [("BLAvq",DList [DInteger (-4),DMap [],DList [DString "zn0"]])])]) @?= "---\nmPztkEPS: {}\nsbn:\n  BLAvq:\n  - -4\n  - {}\n  - - zn0\n"
    , testCase "test10" $
        renderDocument (DList [DMap [("hGu",DMap [("a",DList [DMap [("m",DString ""),("WP",DMap [])]]),("ZlD",DString "7 ")])],DInteger 1,DMap []]) @?= "---\n- hGu:\n    a:\n    - m: ''\n      WP: {}\n    ZlD: '7 '\n- 1\n- {}\n"
    , testCase "test11" $
        renderDocument (DList [DInteger 4,DMap [("s",DInteger (-4)),("GY",DMap [("Mfuxd",DInteger 4),("j",DMap [("enyF",DMap [])]),("c",DString " ")])]]) @?= "---\n- 4\n- s: -4\n  GY:\n    Mfuxd: 4\n    j:\n      enyF: {}\n    c: ' '\n"
    , testCase "test12" $
        renderDocument (DList [DList [],DMap [("n",DList []),("g",DMap [("LR",DInteger (-3))])]]) @?= "---\n- []\n- 'n': []\n  g:\n    LR: -3\n"
    , testCase "test13" $
        renderDocument (DList [DMap [("BjA",DInteger 2),("PL",DString "4  ")],DInteger 3,DString ""]) @?= "---\n- BjA: 2\n  PL: '4  '\n- 3\n- ''\n"
    , testCase "test14" $
        renderDocument (DMap [("n",DList [DString "H"])]) @?= "---\n'n':\n- H\n"
    , testCase "test15" $
        renderDocument (DMap [("N",DList [DMap [("NffY",DInteger (-1))],DList [DInteger (-1)],DInteger 6])]) @?= "---\n'N':\n- NffY: -1\n- - -1\n- 6\n"
    , testCase "test16" $
        renderDocument (DList [DList [DMap [("hZfe",DInteger (-4)),("rjP",DInteger (-4))],DMap [("LO",DInteger 3)],DMap []],DInteger 2]) @?= "---\n- - hZfe: -4\n    rjP: -4\n  - LO: 3\n  - {}\n- 2\n"
    , testCase "test17" $
        renderDocument (DMap [("hy",DList []),("U",DList [DMap [("UC",DList [DString "J"]),("Li",DList [])],DString ""])]) @?= "---\nhy: []\nU:\n- UC:\n  - J\n  Li: []\n- ''\n"
    , testCase "test18" $
        renderDocument (DList [DMap [("rpCvz",DInteger (-8)),("pceg",DMap [("frhHRQ",DList [DInteger 4])])]]) @?= "---\n- rpCvz: -8\n  pceg:\n    frhHRQ:\n    - 4\n"
    , testCase "test19" $
        renderDocument (DMap [("Z",DMap [("NR",DMap [("drL",DString "")]),("Xp",DString "tG "),("FtI",DInteger (-3))]),("GaW",DList [])]) @?= "---\nZ:\n  NR:\n    drL: ''\n  Xp: 'tG '\n  FtI: -3\nGaW: []\n"
    , testCase "test20" $
        renderDocument (DMap [("Y",DList [DMap [("GPQK",DMap [("P",DString " 9nj5")]),("zt",DString "Ve")]]),("Nn",DString "m")]) @?= "---\n'Y':\n- GPQK:\n    P: ' 9nj5'\n  zt: Ve\nNn: m\n"
    , testCase "test21" $
        renderDocument (DList [DString "V ",DList [DMap [("w",DList []),("rUe",DMap [("uIHOr",DString "   ")]),("xjos",DString "")],DMap [("QmAI",DString "Me"),("ojf",DInteger 5),("r",DString " 63z"),("BbeD",DInteger (-1))],DString " "]]) @?= "---\n- 'V '\n- - w: []\n    rUe:\n      uIHOr: '   '\n    xjos: ''\n  - QmAI: Me\n    ojf: 5\n    r: ' 63z'\n    BbeD: -1\n  - ' '\n"
    -- * other primitive types/values
    -- * nested types


-- KODEL PARSERIS NORI KAD RAKTAS BUTU KABUTESE JEIGU JIS N???????????!!!!!!!!


  ]
listOfInts :: String
listOfInts = unlines [
      "---",
      "- 5",
      "- 6"
  ]
dMap :: String
dMap = "---\nfirst: 5\n"

listInList :: String
listInList = unlines[
      "---"
     ,"- - 5"
     ,"  - 6"
  ]
listOfLists :: String
listOfLists = unlines[
      "---"
     ,"- first:"
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
      "first:",
      "  third:",
      "  - 1",
      "second:",
      "  forth:",
      "  - 2"
  ]
dlistInDmap :: String
dlistInDmap = unlines[
      "---",
      "first:",
      "- second:",
      "  - 1",
      "  - 2"
  ]

dlistsInList :: String
dlistsInList = unlines[
      "---",
      "- - - first:",
      "      - 1",
      "      - test",
      "- 3",
      "- null"
  ]

checkTest :: String
checkTest = unlines [
      "---",
      "Coords:",
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