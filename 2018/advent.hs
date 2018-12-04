
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (liftM2, liftM3, msum)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))
-- import qualified Data.Vector as V
-- import Data.Vector ( (//), (!), Vector )
import qualified Data.Set as S
import Data.Tuple (fst, snd, swap)
-- import Data.Bits (xor)
-- import Data.Char (digitToInt, ord)
-- import Data.HashMap (alter, Map, findWithDefault, empty, elems)
-- import qualified Data.HashMap as HM
import Data.List (find, sort, group, tails, inits, mapAccumL, groupBy, sortBy, maximumBy)
import Data.List.Unique (unique, allUnique, sortUniq, repeated, repeatedBy, occurrences, count_)
import Data.List.Zipper hiding (empty)
-- import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe
-- import Data.Monoid (Sum, First, Last)
import Debug.Trace (traceShowId, traceShow)
import Text.Parsec ( many, many1, sepBy, sepBy1, count -- repeats
                   , char, string, noneOf, oneOf, lower -- chars 
                   , try, newline, eof )
import Text.Parsec.String (Parser, parseFromFile)
import Text.Printf (printf)
import Text.Parsec.Number (int, nat)
import Data.Function (on)


-- Parsers
--
getInput :: FilePath -> Parser a -> IO a
getInput path p = do
    result <- parseFromFile (p <* eof) path
    either (error . show) return result

getInput2 :: FilePath -> Parser a -> Parser b -> IO (a, b)
getInput2 path p1 p2 = do
    liftM2 (,) (getInput path p1) (getInput path p2)

integer :: Parser Int
integer = int

parseLines :: Parser a -> Parser [a]
parseLines p = many1 (p <* newline)

integers :: Parser [Int]
integers = parseLines int

tabMatrix :: Parser [[Int]]
tabMatrix = many1 (sepBy1 integer (char '\t') <* newline)

listWords :: Parser [[String]]
listWords = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

wordList :: Parser [ String ]
wordList = parseLines (many1 lower)

squareCuts :: Parser [[Int]]
squareCuts = parseLines cut
  where cut = many1 (many (oneOf "#@,x: ") *> int)

untilNat :: Parser Int
untilNat = many (noneOf "0123456789") *> nat

type Sleep = (Int, Int)
type Shifts = [(Int, [Sleep])]
guards :: Parser Shifts
guards = M.toList . M.fromListWith (++) <$> many shift
  where shift = (,) <$> guard <*> many ((,) <$> try start <*> stop)
        -- some guards are too "aware" for us, skip them
        guard = last <$> many1 (try $ parseNthNatThen 6 " begins shift")
        start = parseNthNatThen 5 "] falls asleep"
        stop  = parseNthNatThen 5 "] wakes up"
        parseNthNatThen i s = last <$> count i untilNat <* string s <* newline

-- Read

readInputFile :: Int -> IO String
readInputFile = readFile . printf "input%02d.txt"

readLine :: Int -> IO String
readLine i = head . lines <$> readInputFile i

infixl 7 % -- modulus has same precedence as (*) or (/)
(%) = mod

day :: (Show b, Show c) => Int -> Parser a -> (a -> b) -> (a -> c) -> IO ()
day n parser p p' = do
    input <- getInput (printf "input%02d.txt" n) parser
    putStrLn $ printf "Day %02d -- %s -- %s" n (show $ p input) (show $ p' input)

main = do
    putStrLn "Start..."
    day 1 integers day01 day01bis
    day 2 wordList day02 day02bis
    day 3 squareCuts hidden day03bis
    day 4 guards day04 day04bis
    day 5 listWords day05 day05bis
    day 6 listWords day06 day06bis
    day 7 listWords day07 day07bis
    day 8 listWords day08 day08bis
    day 9 listWords day09 day09bis
    day 10 listWords day10 day10bis
    day 11 listWords day11 day11bis
    day 12 listWords day12 day12bis
    day 13 listWords day13 day13bis
    day 14 listWords day14 day14bis
    day 15 listWords day15 day15bis
    day 16 listWords day16 day16bis
    day 17 listWords day17 day17bis
    day 18 listWords day18 day18bis
    day 19 listWords day19 day19bis
    day 20 listWords day20 day20bis
    day 21 listWords day21 day21bis
    day 22 listWords day22 day22bis
    day 23 listWords day23 day23bis
    day 24 listWords day24 day24bis
    day 25 listWords day25 day25bis

hidden = const "xxx"

-- Day 01

fromBool :: Bool -> a -> Maybe a
fromBool True  = Just
fromBool False = const Nothing

firstDup :: Ord a => [a] -> a
firstDup = fromJust . msum . snd . mapAccumL seen S.empty
  where seen set x = (x `S.insert` set, fromBool (x `S.member` set) x)

day01 :: [Int] -> Int
day01 = sum

day01bis :: [Int] -> Int
day01bis = firstDup . scanl (+) 0 . cycle


-- Day 02

day02 :: [String] -> Int
day02 = uncurry (*) . (has 3 &&& has 2)
  where has x = sum . map (min 1 . length . repeatedBy (== x))

day02bis :: [String] -> String
day02bis = snd . firstDup . concatMap (zip [0..] . delOne [])
  where delOne :: String -> String -> [String]
        delOne prefix (x:xs) = (prefix ++ xs) : delOne (prefix ++ [x]) xs
        delOne prefix [] = []


-- Day 03

day03 :: [[Int]] -> Int
day03 = length . repeated . concatMap positions
  where positions [_, x, y, w, h] = [(px, py) | px <- [x..x+w-1], py <- [y..y+h-1]]

day03bis :: [[Int]] -> Int
day03bis xs = head $ head $ filter (\x -> not (any (conflict x) xs)) xs
  -- Not fast enough:
  --    conflict x y = if x == y then False else day03 [x, y] > 0
  where conflict a@[_, x, y, w, h] b@[_, x', y', w', h']
          | a == b = False
          | otherwise = x < x'+w' && x' < x+w && y < y'+h' && y' < y+h


-- Day 04

maximumBy' :: Ord b => (a -> b) -> [a] -> a
maximumBy' = maximumBy . (compare `on`)

mostCommon :: Ord a => [a] -> a
mostCommon = fst . last . count_

day04 :: Shifts -> Int
day04 = uncurry (*) . (id *** mostCommon . minutes) . maximumBy' lazyness
  where minutes = concatMap (\(x, y) -> [x..y-1])
        lazyness = sum . map (uncurry subtract) . snd

day04bis :: Shifts -> Int
day04bis = uncurry (*) . (id *** snd) . maximumBy' snd . map (id *** countSleeps)
  where minutes = concatMap (\(m, m') -> [m..m'-1])
        -- returns (maxCount, mostFreqMinute)
        countSleeps = swap . last . count_ . minutes


-- Day 05

day05 = const "Not implemented"
day05bis = const "Not implemented"

day06 = const "Not implemented"
day06bis = const "Not implemented"

day07 = const "Not implemented"
day07bis = const "Not implemented"

day08 = const "Not implemented"
day08bis = const "Not implemented"

day09 = const "Not implemented"
day09bis = const "Not implemented"

day10 = const "Not implemented"
day10bis = const "Not implemented"

day11 = const "Not implemented"
day11bis = const "Not implemented"

day12 = const "Not implemented"
day12bis = const "Not implemented"

day13 = const "Not implemented"
day13bis = const "Not implemented"

day14 = const "Not implemented"
day14bis = const "Not implemented"

day15 = const "Not implemented"
day15bis = const "Not implemented"

day16 = const "Not implemented"
day16bis = const "Not implemented"

day17 = const "Not implemented"
day17bis = const "Not implemented"

day18 = const "Not implemented"
day18bis = const "Not implemented"

day19 = const "Not implemented"
day19bis = const "Not implemented"

day20 = const "Not implemented"
day20bis = const "Not implemented"

day21 = const "Not implemented"
day21bis = const "Not implemented"

day22 = const "Not implemented"
day22bis = const "Not implemented"

day23 = const "Not implemented"
day23bis = const "Not implemented"

day24 = const "Not implemented"
day24bis = const "Not implemented"

day25 = const "Not implemented"
day25bis = const "Not implemented"

