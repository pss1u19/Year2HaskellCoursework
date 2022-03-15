{-# LANGUAGE DeriveGeneric, PolyKinds #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020

-- Author pss1u19 student ID 31163084 student ad University of Southampton
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
{-# LANGUAGE DeriveGeneric, PolyKinds #-}
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

-- END OF CODE YOU MUST NOT MODIFY



-- ADD YOUR OWN CODE HERE

-- Challenge 1 --


solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
-- if the list of words is empty we return emtpty list as the function has done its job
solveWordSearch [] _ = []
solveWordSearch ( word:wrds ) ws =

  let
    -- function for easy access to the grid's size
    gridSize = length ws
  in
  let
      -- function for getting the letter from the grid at coordinates sent as arguments (x,y)
      getL (x,y)          | x < 0 = '@'
                          | y < 0 = '@'
                          | x >= gridSize = '@'
                          | y >= gridSize = '@'
                          | otherwise = ( ws !! y ) !! x

  in
  let
    -- functiont to check whether the sent word is in Forward Orientation starting from the given coordinates
    checkF [] _ = True                                                        -- if there are no more letters in the word to check we return true as the word is in Forward orientation
    checkF (c:cs) (x,y)   | x > gridSize - 1 = False                          -- if the check function exceeds the grid the word is not in this orientation and returns False
                          | c == getL (x,y) = checkF cs ( x + 1 , y )         -- if the letter of the word matches the letter from the grid we countnue and check the rest of the word
                          | otherwise = False                                 -- otherwise the function returns False
  in
  -- The other check* functions are build the same way
  let
    checkB [] _ = True
    checkB (c:cs) (x,y)   | x > gridSize - 1 = False
                          | c == getL ( x , y ) = checkB cs ( x - 1, y )
                          | otherwise = False
  in
  let
    checkU [] _ = True
    checkU (c:cs) (x,y)   | y < 0 = False
                          | c == getL ( x , y ) = checkU cs ( x , y - 1 )
                          | otherwise = False
  in
  let
    checkD [] _ = True
    checkD (c:cs) (x,y)   | y > gridSize - 1 = False
                          | c == getL ( x , y ) = checkD cs ( x , y + 1 )
                          | otherwise = False
  in
  let
    checkUF [] _ = True
    checkUF (c:cs) (x,y)    | x > gridSize - 1 = False
                            | y < 0 = False
                            | c == getL ( x , y ) = checkUF cs ( x + 1 , y - 1 )
                            | otherwise = False
  in
  let
    checkUB [] _ = True
    checkUB (c:cs) (x,y)    | x < 0 = False
                            | y < 0 = False
                            | c == getL ( x , y ) = checkUB cs ( x - 1 , y - 1 )
                            | otherwise = False
  in
  let
    checkDF [] _ = True
    checkDF (c:cs) (x,y)    | x > gridSize - 1 = False
                            | y > gridSize - 1 = False
                            | c == getL ( x , y ) = checkDF cs ( x + 1 , y + 1 )
                            | otherwise = False
  in
  let
    checkDB [] _ = True
    checkDB (c:cs) (x,y)    | x < 0 = False
                            | y > gridSize - 1 = False
                            | c == getL ( x , y ) = checkDB cs ( x - 1 , y + 1 )
                            | otherwise = False
  in
  let
    -- Function checking each possible orientation for a word at starting coordinates both given as arguments
    getOrientation w pos    | checkU w pos == True = Just ( pos , Up)
                            | checkF w pos == True = Just ( pos , Forward )
                            | checkD w pos == True = Just ( pos , Down )
                            | checkB w pos == True = Just ( pos , Back )
                            | checkUF w pos == True = Just ( pos , UpForward )
                            | checkDF w pos == True = Just ( pos , DownForward )
                            | checkUB w pos == True = Just ( pos , UpBack )
                            | checkDB w pos == True = Just ( pos , DownBack )
                            | otherwise = Nothing
  in
  let
    -- function for going through the grid checking the Orientation of the word given as argument and returning its Placement
    findWord w (x,y)    | ( getL (x,y) ) /= ( w !! 0 ) = if x == gridSize - 1 then findWord w ( 0 , y + 1 ) else findWord w ( x + 1 , y )   --if the letter the function is on doesnt match the first letter of the word there is no point in checking each orientation
                        | getOrientation w (x,y) /= Nothing = getOrientation w (x,y)
                        | x == gridSize - 1 = findWord w ( 0 , y + 1 )
                        | otherwise = findWord w ( x + 1 , y )
  in
   (word,findWord word (0,0)):solveWordSearch wrds ws   -- Combinig the word and its Placement into a pair. Then concatinating it to the rest of the list found by recursion with the rest of the list or words.

-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

--exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
--exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]


-- Challenge 2 --
createWordSearch :: [String] -> Double -> IO WordSearchGrid
createWordSearch [] _ = return []
createWordSearch words d =
                do startingGrid <- getListRChars uniqueCharSet sizeOfTheGrid                  --start by generating a grid with random chars from the uniqueCharSet
                   wordsWithPlacement <- genRWords words numOfRows [] words                   --get random Placements for the words
                   return ( finalizeGrid startingGrid wordsWithPlacement numOfRows )          --finalize the grid by combinig the words with the staring grid
      where charSet = concat words                                                            --for easy access to all the chars
            biggestToSmallest = sortBy (\w1 w2 -> compare (length w2) (length w1)) words      --for easy access to the sorted list of words
            numOfChar = length charSet                                                        --for easy access to the number of chars
            uniqueCharSet = nub charSet                                                       --for easy access to all the unique chars
            biggestWordSize = length (biggestToSmallest!!0)                                   --for easy access to the biggest word's size
            minGridSize = max (biggestWordSize*biggestWordSize) (ceiling $ (fromIntegral numOfChar)/d)    --for easy access to the minimal grid size
            numOfRows = sizeOfGrid minGridSize 0                                              --for easy access to the number of rows
            sizeOfTheGrid = numOfRows*numOfRows                                               --for easy access


--Useful small helping funtions
indexFromPos :: Posn -> Int -> Int
indexFromPos (x,y) i = i*y + x

placeCharAt :: [a] -> Int -> a -> [a]
placeCharAt xs n el = take n xs ++ [el] ++ drop (n + 1) xs

listIntoGrid :: Int -> [a] -> [[a]]
listIntoGrid i ws = takeWhile (not.null) $ unfoldr (Just . splitAt i) ws

joinLists [] ys = ys
joinLists (x:xs) ys = x:joinLists ys xs

deteteAt :: Int -> [a] -> [a]
deteteAt i cs = take i cs ++ drop (1 + i) cs

sizeOfGrid :: Int -> Int -> Int
sizeOfGrid mSize i
    | i*i<mSize = sizeOfGrid mSize (i+1)
    | otherwise = i

finalizeGrid :: [Char] -> [(Posn,Char)] -> Int -> WordSearchGrid        --function for finalizing the grid by inserting the words into the starting grid
finalizeGrid ws [] size = listIntoGrid size ws
finalizeGrid ws ( ( pos, char ):posNCharList) size= finalizeGrid (placeCharAt ws (indexFromPos pos size) char) posNCharList size

getListRChars :: [Char] -> Int -> IO [Char]    --returns list of chars to be used for the starting grid
getListRChars charSet 0 = return []
getListRChars charSet reps = do
    c <- (randomRIO (0, length charSet - 1))
    cs <- getListRChars charSet (reps-1)
    return (chars!!c:cs)


genRWords :: [String] -> Int -> [(Posn,Char)] -> [String] -> IO [(Posn,Char)]   --function for giving the Words random positions and returning it as a list of positions of each letter
genRWords [] size posNCharList origWords = return posNCharList
genRWords (c:cs) size posNCharList origWords= do
    newList <- genRPosNCharList c size 0 posNCharList [] (c:cs)
    if newList /= []
       then genRWords cs size newList origWords
       else genRWords origWords size [] origWords

genRPosNCharList :: String -> Int -> Int -> [(Posn,Char)] -> [Posn] -> [String] -> IO [(Posn,Char)]   --fuction for getting a word a random Placement and returning it as list of letters and their positions in the grid
genRPosNCharList wrd size triedPosnsSize posNCharList triedPosns wrds = do
    y <- (randomRIO (0, size-1))
    x <- (randomRIO (0, size-1))
    if notElem (x,y) triedPosns
       then do let getorien =  getPossibleOrientations wrd (x,y) size
               if getorien /= []
                  then do ro <- tryWordWithOrien wrd (x,y) size posNCharList getorien triedPosns wrds
                          if ro /= []
                             then return ro
                             else genRPosNCharList wrd size (triedPosnsSize+1) posNCharList ((x,y):triedPosns) wrds
                  else genRPosNCharList wrd size (triedPosnsSize+1) posNCharList ((x,y):triedPosns) wrds
       else if triedPosnsSize >= (size*size)
                then return []
                else genRPosNCharList wrd size (triedPosnsSize+1) posNCharList ((x,y):triedPosns) wrds


tryWordWithOrien :: String -> Posn -> Int -> [(Posn,Char)] -> [Orientation] -> [Posn] -> [String] -> IO [(Posn,Char)]     --trying to pair a word with an orientation so that the word is in the grid
tryWordWithOrien wrd pos size posNCharList (o:os) triedPosns wrds =
    do
      tryorien <- (randomRIO (0,(length (o:os)) -1))
      let try = examineOrientation posNCharList pos wrd ((o:os)!!tryorien) []
      let removed = deteteAt tryorien (o:os)
      if (try /= []) then return try
          else if (removed /= [] )   then tryWordWithOrien wrd pos size posNCharList removed triedPosns wrds
                else return []


examineOrientation :: [(Posn,Char)] -> Posn -> String -> Orientation -> [(Posn,Char)] -> [(Posn,Char)]
examineOrientation posNCharList (x,y) [] o triedPsnNChars = joinLists triedPsnNChars posNCharList
examineOrientation posNCharList (x,y) (c:cs) o triedPsnNChars
    | (o==Forward && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x+1,y) cs o (((x,y),c):triedPsnNChars)
    | (o==Back && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x-1,y) cs o (((x,y),c):triedPsnNChars)
    | (o==Up && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x,y-1) cs o (((x,y),c):triedPsnNChars)
    | (o==Down && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x,y+1) cs o (((x,y),c):triedPsnNChars)
    | (o==UpForward && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x+1,y-1) cs o (((x,y),c):triedPsnNChars)
    | (o==UpBack && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x-1,y-1) cs o (((x,y),c):triedPsnNChars)
    | (o==DownForward && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x+1,y+1) cs o (((x,y),c):triedPsnNChars)
    | (o==DownBack && (examineLetter posNCharList (x,y) c)) = examineOrientation posNCharList (x-1,y+1) cs o (((x,y),c):triedPsnNChars)
    | otherwise = []

examineLetter :: [(Posn,Char)] -> Posn -> Char -> Bool    --
examineLetter [] _ _ = True
examineLetter posNCharList pos c
    | (i == Nothing || examineLetter' i c posNCharList) = True
    | otherwise = False
        where i = findIndex (\x -> fst x == pos) posNCharList

examineLetter' :: Maybe Int -> Char -> [(Posn,Char)] -> Bool
examineLetter' Nothing _ _ = True
examineLetter' (Just i) char posNCharList = (snd (posNCharList !! i)) == char


getPossibleOrientations :: String -> Posn -> Int -> [Orientation]
getPossibleOrientations wrd (x,y) gridsize =
    getPossibleOrientations' 1 d u f b []
      where d = (gridsize - y) >= length wrd
            u = (y + 1) >= length wrd
            f = (gridsize - y) >= length wrd
            b = (y + 1) >= length wrd

getPossibleOrientations' :: Int -> Bool -> Bool -> Bool -> Bool -> [Orientation]  -> [Orientation]
getPossibleOrientations' n d u f b possibleOrientations
    | (n==1 && u)            = getPossibleOrientations' 2 d u f b (Up:possibleOrientations)
    | (n==2 && d)            = getPossibleOrientations' 3 d u f b (Down:possibleOrientations)
    | (n==3 && b)            = getPossibleOrientations' 4 d u f b (Back:possibleOrientations)
    | (n==4 && f)            = getPossibleOrientations' 5 d u f b (Forward:possibleOrientations)
    | (n==5 && u && f)       = getPossibleOrientations' 6 d u f b (UpForward:possibleOrientations)
    | (n==6 && u && b)       = getPossibleOrientations' 7 d u f b (UpBack:possibleOrientations)
    | (n==7 && d && f)       = getPossibleOrientations' 8 d u f b (DownForward:possibleOrientations)
    | (n==8 && d && b)       = getPossibleOrientations' 9 d u f b (DownBack:possibleOrientations)
    | n==9 = possibleOrientations
    | otherwise = getPossibleOrientations' (n+1) d u f b possibleOrientations
--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do
                                        g <- createWordSearch words maxDensity
                                        let soln = solveWordSearch words g
                                        printGrid g
                                        return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint _ = ""

-- examples in the instructions
--ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
--ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
--ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
--ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))


-- Challenge 4 --

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
--exId =  (LamAbs 1 (LamVar 1))
--ex5'1 = (LamApp (LamVar 1) (LamVar 2))
--ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
--ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
--ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing)

-- Examples in the instructions

-- (\x1 -> x1 x2)
--ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F
--ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)
--ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)
--wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
--ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4)
--ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))
--ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
--ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") )
