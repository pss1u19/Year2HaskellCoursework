module Tests where
  -- (c) University of Southampton 2020
  -- Author pss1u19 student ID 31163084 student ad University of Southampton
import Challenges

import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random


test :: [String] -> WordSearchGrid -> [(String, Maybe Placement)] -> IO ()
test wrds ws ans  | comp ans (solveWordSearch wrds ws) == True = putStrLn "test  Passed"
                  | otherwise = putStrLn "test Failed"

comp :: [(String, Maybe Placement)]-> [(String, Maybe Placement)] -> Bool
comp [] _ = True
comp (a:ans) (t:tem)  | a == t = comp ans tem
                      | otherwise = False


                      {-
printWordWithPLacement :: [(String, Maybe Placement)] -> IO ()
printWordWithPLacement [] = return ()
printWordWithPLacement ((str,((x,y),o)):ws) =
                    do
                      putStrLn (str ++" is at coordinates " ++(show x) ++" , " ++ (show y) ++ " and orientation " ++ (show o))
                      printWordWithPLacement ws
                      -}
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
main :: IO()
main = do
  putStrLn "test Starts"
  test exWords1'2 exGrid1'2 [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),("MELON",Just ((7,8),Up)),("RASPBERRY",Just ((8,0),DownBack)),("APPLE",Just ((2,8),UpForward)),("PLUM",Just ((5,1),DownBack)),("GRAPE",Just ((8,6),Up))]
  test exWords1'1 exGrid1'1 [("HASKELL",Just ((0,0),DownForward)),("STRING",Just ((7,0),Back)),("STACK",Just ((2,2),Forward)),("MAIN",Just ((2,7),Up)),("METHOD",Just ((4,3),Down))]
  putStrLn "test Ending"
