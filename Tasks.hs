-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Numeric
import Text.Printf

import Data.Char

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
{-floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

goodinput=tail [["Name","10","11","12","13","14","15","16","17"],["Olivia Noah","373","160","151","0","0","0","0","0"],["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","45","8","0","0","0","0","0","0"]] 
names=map head goodinput
num=map tail goodinput
num1=map (map (read::String->Int)) num
average=map operation num1 
final=["Name","Average Number of Steps"] : (oneList names average)
-}

oneList :: [String] -> [String] -> [[String]]
oneList [] _ = []
oneList (x:xs) (y:ys) = [[x,y]] ++ (oneList xs ys)

operation :: [Int] -> String
operation l = printf "%.2f" ((fromIntegral (foldr (+) 0 l))/(fromIntegral 8) :: Float)  

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : (oneList (map head (tail m)) (map operation (map (map (read::String->Int)) (map tail (tail m)))))

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = goals (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))

goals l = length(filter (>1000) l)
{-goodinput=tail [["Name","10","11","12","13","14","15","16","17"],["Olivia Noah","373","160","151","0","0","0","0","0"],["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","45","8","0","0","0","0","0","0"]]
    num=map tail goodinput
    num1=map (map (read::String->Int)) num
    sum=map (foldr (+) 0) num1
    counts= goals sum

    -}

{-length goodinput-}
-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m))/(fromIntegral (length(tail m)))


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral (foldr (+) 0 (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))))/(fromIntegral (length(tail m)))
{-goodinput=tail [["Name","10","11","12","13","14","15","16","17"],["Olivia Noah","373","160","151","0","0","0","0","0"],["Riley Jackson","31","0","0","7","0","0","0","0"],["Emma Aiden","45","8","0","0","0","0","0","0"]]
    num=map tail goodinput
    num1=map (map (read::String->Int)) num
    sum=map (foldr (+) 0) num1
    sum1=foldr (+) 0 sum-}


-- Task 3

printString :: [Float] -> [String]
printString l = map (\x -> printf "%.2f" x) l

get_num :: Table -> [[Int]]
get_num m = map (map (read::String->Int)) (map tail (tail m))

get_sum :: [[Int]] -> [Float]
get_sum m = map (/(fromIntegral (length m))) (map (fromIntegral) (foldr (\xs ys -> zipWith (+) xs ys) [0,0,0,0,0,0,0,0] m))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (printString ((get_sum (get_num m))) : [])

{-goodinput=[["Name","10","11","12","13","14","15","16","17"],["Olivia Noah","373","160","151","0","0","0","0","0"],["Riley Jackson","31","0","0","7","0","0","0","0"],["Emma Aiden","45","8","0","0","0","0","0","0"]]
num=map tail goodinput
num1=map (map (read::String->Int)) num
sum=foldr (\xs ys -> zipWith (+) xs ys) [0,0,0,0,0,0,0,0] num1
sum1=map (fromIntegral) sum
divizor=[10 :: Float,11,12,13,14,15,16,17]
fin=zipWith (/) sum1 divizor
printString fin

-}

-- Task 4
{-
a=tail input
a1=map (drop 3) a
a2=map (map (read::String->Int)) a1
l1= map head a2
l2= map (!! 1) a2
l3= map last a2
end1=["VeryActiveMinutes"] ++ printString (map fromIntegral [range1 l1,range2 l1,range3 l1])
end2=["FairlyActiveMinutes"] ++ printString (map fromIntegral [very l2,fairly l2,lightly l2])
end3=["LightlyActiveMinutes"] ++ printString (map fromIntegral [very l3,fairly l3,lightly l3])
["column","range1","range2","range3"]:end1:end2:end3:[]

-}

printString2 :: [Float] -> [String]
printString2 l = map (\x -> printf "%.0f" x) l

get3 :: Table -> [[Int]]
get3 m = map (map (read::String->Int)) (map (drop 3) (tail m))

count_workout :: [Int] -> [[Char]]
count_workout m = printString2 (map fromIntegral [range1 m,range2 m,range3 m])

range1 l = length(filter (>=0) (filter (<50) l))
range2 l = length(filter (<100) (filter (>=50) l)) 
range3 l = length(filter (<500) (filter (>=100) l))  

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:(["VeryActiveMinutes"] ++ count_workout (map head (get3 m))):(["FairlyActiveMinutes"] ++ count_workout (map (!! 1) (get3 m))):(["LightlyActiveMinutes"] ++ count_workout (map last (get3 m))):[]

-- Task 5

cmp_steps a b  
  | ((read::String->Int) (((!! 1)) a)) < ((read::String->Int) (((!! 1)) b)) = LT
  | ((read::String->Int) (((!! 1)) a)) > ((read::String->Int) (((!! 1)) b)) = GT
  | otherwise = cmp_people a b

cmp_people a b
  | (head a) < (head b) = LT
  | (head a) > (head b) = GT


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (sortBy cmp_steps (map (take 2) (tail m)))
{- 
a=tail input

sortBy cmp_steps b
-}

cmp_diff a b  
  | ((read::String->Double) (((!! 3)) a)) < ((read::String->Double) (((!! 3)) b)) = LT
  | ((read::String->Double) (((!! 3)) a)) > ((read::String->Double) (((!! 3)) b)) = GT
  | otherwise = cmp_people a b

-- Task 6
operation1 :: [Int] -> Float
operation1 l = ((fromIntegral (foldr (+) 0 l))/(fromIntegral 4) :: Float)  

get_first :: [[String]] -> [[Int]]
get_first m = map(map (read::String->Int)) (map (take 4) m)

get_last :: [[String]] -> [[Int]]
get_last m = map(map (read::String->Int)) (map (drop 4) m)

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : (sortBy cmp_diff (zipWith (++) (oneList (map head (tail m)) (printString (map operation1 (get_first (map tail (tail m)))))) (oneList (printString (map operation1 (get_last (map tail (tail m))))) (printString (map abs (zipWith (-) (map operation1 (get_first (map tail (tail m)))) (map operation1 (get_last (map tail (tail m))))))))))
{-a=tail input
  names=map head (tail input)
  num=map tail a
  first= map (take 4) num
  last= map (drop 4) num
  first1=map(map (read::String->Int)) first
  last1=map(map (read::String->Int)) last
  l1=map operation1 first1
  l2=map operation1 last1
  diff=zipWith (-) l1 l2
  diff1=map abs diff
  fin1=printString l1 
  fin2=printString l2
  fin3=printString diff1
  end1=oneList names fin1
  end2=oneList fin2 fin3
  zipWith (++) end1 end2

  ["Olivia Noah","171.00","0.00","171.00"]
  ["Olivia Noah","373","160","151","0","0","0","0","0"], 
  -}

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8
{-type Value = String
type Row = [Value]-}
-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = undefined
{-findIndex (==3) [0,2,4,6,8]-}

get_sleep_total :: Row -> Row
get_sleep_total r = [head r] ++ (printString [fromIntegral (foldr (+) 0 (map (read::String->Int) (tail r)))]) ++ []
{-
a=tail input 
mail=head input
a1=map (read::String->Int) nums
sum=foldr (+) 0 a1
[mail] ++ (printString [fromIntegral sum]) ++ []

-}