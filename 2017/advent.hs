 {-# LANGUAGE UnicodeSyntax #-}

import Control.Monad (liftM2, liftM3)
import Control.Applicative (liftA2)
-- import Data.AdditiveGroup ((^+^), 
import qualified Data.Vector as V
import Data.Vector ( (//), (!), Vector )
import qualified Data.Set as S
import Data.Bits (xor)
import Data.Char (digitToInt, ord)
import Data.HashMap (alter, Map, findWithDefault, empty, elems)
import qualified Data.HashMap as HM
import Data.List (find, sort, group)
import Data.List.Split (chunksOf)
import Data.List.Unique (unique, allUnique, repeated, sortUniq, count)
import Data.List.Zipper hiding (empty)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Sum )
import qualified Data.Set as Set
import Debug.Trace (traceShowId, traceShow)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Prelude hiding (null)
import Text.Parsec hiding (count)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Printf (printf)


-- Parsers
--
withInput :: FilePath -> Parser a -> IO a
withInput path p = do
    result <- parseFromFile (p <* eof) path
    either (error . show) return result

withInput2 :: FilePath -> Parser a -> Parser b -> IO (a, b)
withInput2 path p1 p2 = do
    liftM2 (,) (withInput path p1) (withInput path p2)

integer :: Parser Int
integer = read <$> (negative <|> number)
    where negative = (:) <$> char '-' <*> number
          number = many1 digit

number :: Parser Int
number = read <$> (many1 digit)

tabMatrix :: Parser [[Int]]
tabMatrix = many1 (sepBy1 number (char '\t') <* newline)

listWords :: Parser [[String]]
listWords = many1 (sepBy1 (many1 lower) (char ' ') <* newline)


-- Read
--
readInputFile :: Int -> IO String
readInputFile = readFile . printf "input%02d.txt"

readLine :: Int -> IO String
readLine i = head . lines <$> readInputFile i

main = do
    day01main
    day02main
    day03main
    day04main
    day05main
    day06main
    day07main
    day08main
    day09main
    day10main
    day11main
    day12main
    putStrLn "Done !"

infixl 7 % -- modulus has same precedence as (*) or (/)
(%) = mod


-- day01

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

day01 :: [Int] -> Int
day01 input = sum $ map fst $ filter (uncurry (==)) $ zip input (shift input)

shiftN :: Int -> [a] -> [a]
shiftN 0 = id
shiftN n = (shiftN (n-1)) . shift

day01bis :: [Int] -> Int
day01bis input = sum $ map fst $ filter (uncurry (==)) $ zip input (shiftN (length input `div` 2) input)

digitList :: Parser [Int]
digitList = many1 (digitToInt <$> digit) <* newline

day01main :: IO ()
day01main = withInput "input01.txt" digitList >>= \digits -> do
    putStrLn $ printf "Day 01 -- %d -- %d" (day01 digits) (day01bis digits)


-- day02
--
difference :: Num a => Ord a => [a] -> a
difference l = maximum l - minimum l

day02 :: [[Int]] -> Int
day02 = sum . map difference

cartProd :: [a] -> [b] -> [(a, b)]
cartProd = liftM2 (,)

divides :: (Int, Int) -> Bool
divides (a, b) = a > b && 0 == mod a b 

evenDivide :: [Int] -> Int
evenDivide l = uncurry div $ head $ filter divides $ cartProd l l

day02bis :: [[Int]] -> Int
day02bis = sum . map evenDivide

day02main = withInput "input02.txt" tabMatrix >>= \numbers -> do
    putStrLn $ printf "Day 02 -- %d -- %d" (day02 numbers) (day02bis numbers)


-- day 03
--
-- 1*1 - 3*3 - 5*5 - 7*7
-- 1   - 9   - 25  - 49
-- 1   - 8   - 16  - 24
-- 0   - 8   - 8   - 8

data Inc = Inc | Dec  deriving Show
data Direction = N | S | O | E deriving Show
type Pos = (Int, Int)
data Cell = Cell {pos :: Pos, dist :: Int, num :: Int} deriving Show
data Ack = Ack {dir :: Direction, len :: Int, i :: Int, inc :: Inc, cell :: Cell} deriving Show

move :: Direction -> Pos -> Pos
move N (x, y) = (x, y+1)
move S (x, y) = (x, y-1)
move O (x, y) = (x-1, y)
move E (x, y) = (x+1, y)

turnLeft :: Direction -> Direction
turnLeft N = O
turnLeft O = S
turnLeft S = E
turnLeft E = N

positions :: [Cell]
positions = cell <$> iterate next' (Ack E (0 - 1) 0 Inc (Cell (0,0) 0 1))

switch :: Inc -> Inc
switch Inc = Dec
switch Dec = Inc

change :: Inc -> Int -> Int
change Inc n = (n + 1)
change Dec n = (n - 1)

next' :: Ack -> Ack
next' (Ack E   len 0 Inc (Cell pos dist num)) = Ack N (len + 1) (len + 1) Dec (Cell (move E pos) (dist + 1) (num + 1))
next' (Ack dir len 0 Inc (Cell pos dist num)) = Ack (turnLeft dir) len len Dec (Cell (move (turnLeft dir) pos) (dist - 1) (num + 1))
next' (Ack dir len 0 Dec (Cell pos dist num)) = Ack dir len len Inc (Cell (move dir pos) (dist + 1) (num + 1))
next' (Ack dir len i inc (Cell pos dist num)) = Ack dir len (i - 1) inc (Cell (move dir pos) (change inc dist) (num + 1))


genCorners :: Int -> Int -> [Int]
genCorners n inc = n : genCorners (n + inc) (inc + 8)

day03 :: Int -> Int
day03 n = dist . fromJust $ find ((== n) . num) positions


positionOf :: Int -> Pos
positionOf n = positions' !! (n - 1)

positions' :: [Pos]
positions' = (\(p, _, _) -> p) <$> iterate next ((0, 0), S, S.singleton (0, 0))
  where
    next (pos, dir, seen) =
      if leftPos `S.notMember` seen
        then (leftPos, left, S.insert leftPos seen)
        else (move dir pos, dir, S.insert (move dir pos) seen)
      where
        leftPos = move left pos
        left = turnLeft dir

sumValues :: [Int]
sumValues = go M.empty positions'
  where
    go _ [] = []
    go sums ((0, 0):rest) = 1 : go (M.insert (0, 0) 1 sums) rest
    go sums (pos:rest) = thisVal : go (M.insert pos thisVal sums) rest
      where
        thisVal =
          sum . catMaybes . fmap (`M.lookup` sums) . neighbourPositions $ pos


neighbourPositions :: Pos -> [Pos]
neighbourPositions (x, y) =
  [ pos
  | dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  , let pos = (x + dx, y + dy)
  , pos /= (x, y)
  ]

day03bis :: Int -> Int
day03bis n = fromJust $ find (> n) sumValues

day03main = do
    putStrLn $ printf "Day 03 -- %d -- %d" (day03 277678) (day03bis 277678)


-- day 04

day04 :: [[String]] -> Int
day04 = length . filter allUnique

day04bis :: [[String]] -> Int
day04bis = length . filter allUnique . map (map sort)

day04main :: IO ()
day04main = withInput "input04.txt" listWords >>= \input -> do
    putStrLn $ printf "Day 04 -- %d -- %d" (day04 input) (day04bis input)


-- day 05

intLines :: Parser [Int]
intLines = many1 (integer <* newline)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Zip (a:ls) rs) = Just $ Zip ls (a:rs)
goLeft _ = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Zip ls (a:b:rs)) = Just $ Zip (a:ls) (b:rs)
goRight _ = Nothing

next :: (Int -> Int) -> Maybe (Zipper Int) -> Maybe (Zipper Int)
next inc = (>>= update)
    where 
        update z = let offset = cursor z
                       incremented = replace (inc offset) z
                    in move offset (Just incremented)
        move n z 
            | n > 0 = iterate (>>= goRight) z !! n
            | n < 0 = iterate (>>= goLeft) z !! (-n)
            | otherwise = z

computeSteps :: (Int -> Int) -> [Int] -> Int
computeSteps inc = length . takeWhile isJust . iterate (next inc) . Just . fromList

day05 :: [Int] -> Int
day05 = computeSteps (+1)

day05bis :: [Int] -> Int
day05bis = computeSteps (\n -> if n >= 3 then n - 1 else n + 1)

day05main :: IO ()
day05main = withInput "input05.txt" intLines >>= \input -> do
    putStrLn "Day 05 -- skipped"
    -- putStrLn $ printf "Day 05 -- %d -- %d" (day05 input) (day05bis input)


-- day 06

tabLine :: Parser (Vector Int)
tabLine = V.fromList <$> sepBy1 number (char '\t') <* newline

distribute :: Vector Int -> Vector Int
distribute array = V.accum (+) array updates
    where
        updates = (maxId, - max):[ (idx % len, 1) | idx <- take max [maxId + 1..] ]
        max = array ! maxId
        maxId = V.maxIndex array
        len = V.length array

dup :: Ord a => [a] -> (Int, Int)
dup xs = dup' 0 xs M.empty
    where 
        dup' i (x:xs) m = case M.lookup x m of
                          Just n -> (i, i - n)
                          Nothing -> dup' (i + 1) xs (M.insert x i m)

day06 :: Vector Int -> Int
day06 = fst . dup . iterate distribute

day06bis :: Vector Int -> Int
day06bis = snd . dup . iterate distribute

day06main :: IO ()
day06main = withInput "input06.txt" tabLine >>= \input -> do
    putStrLn $ printf "Day 06 -- %d -- %d" (day06 input) (day06bis input)

-- Day 07

data Program = Prog {name :: String, weight :: Int, children :: [String]} deriving Show

programs :: Parser [Program]
programs = many1 program
    where program = liftM3 Prog name weight children
          name = many1 lower 
          weight = string " (" *> number <* char ')'
          children = (option [] childrenList) <* newline
          childrenList = string " -> " *> sepBy1 name (string ", ")

day07 :: [Program] -> String
day07 ps = head . unique $ programs ++ allChildren
    where programs = map name ps
          allChildren = children =<< ps

-- TODO
-- day07bis :: [Program] -> Int
-- day07bis ps = drill 0 (findX $ day07 ps)
--     where
--         findX n = filter (\p -> n == name p) ps !! 0
--         getChildren = map findX . children 
--         sumWeight p = (weight p) + (sum . map sumWeight . getChildren $ p)
--         check p = head . unique $ (traceShowId . map sumWeight $ (getChildren p))
--         getWrongChild p = filter (\p -> weight p == getWrongWeight p) ps !! 0
--         getWrongWeight p = check p
--         getGoodWeight p = head . repeated $ (traceShowId . map sumWeight $ (getChildren p))
--         sumWeights p = map sumWeight $ getChildren p
--         drill n p = if 1 == (length $ sortUniq $ sumWeights p) then n
--                     else drill (getGoodWeight p) (traceShowId $ getWrongChild p)


day07main :: IO ()
day07main = withInput "input07.txt" programs >>= \input -> do
    putStrLn $ printf "Day 07 -- %s -- %d" (day07 input) (0 :: Int) --(day07bis input)

-- day 08

type Cond = (String, (Int -> Bool))
type Instr = (String, (Int -> Int), Cond)

instructions :: Parser [Instr]
instructions = many1 (instruction <* newline)
instruction :: Parser Instr
instruction = liftM3 (,,) name change cond
    where
        s = char ' '
        name = many1 lower <* s
        inc = applyChange <$> name
        change = liftM2 ($) inc (integer <* s)
        cond = (,) <$> (string "if " *> name) <*> (test)
        test = liftM2 ($) compare integer
        compare = applyCond <$> (many1 (oneOf "<>=!") <* s)

        applyChange x = flip $ case x of
                                    "inc" -> (+)
                                    "dec" -> (-)
        applyCond x = flip $ case x of 
                                  "<=" -> (<=)
                                  "<"  -> (<)
                                  ">=" -> (>=)
                                  ">"  -> (>)
                                  "==" -> (==)
                                  "!=" -> (/=)

type Reg = Map String Int
eval :: Reg -> Instr -> Reg
eval reg (var, inc, (tvar, cond)) 
    | cond (reg `get` tvar) = alter increment var reg
    | otherwise = reg
    where 
        get = flip (findWithDefault 0)
        increment val = Just $ inc (fromMaybe 0 val)

day08 :: [Instr] -> Int
day08 = maximum . elems . foldl eval empty

day08bis :: [Instr] -> Int
day08bis = maximum . concatMap elems . scanl eval empty

day08main :: IO ()
day08main = withInput "input08.txt" instructions >>= \input -> do
    putStrLn $ printf "Day 08 -- %d -- %d" (day08 input) (day08bis input)

-- day 09

data Group = Groups [ Group ] | Garbage String deriving Show

river :: Parser Group
river = group <* newline
    where group = Groups <$> between (char '{') (char '}') groupContent
          groupContent = sepBy (group <|> garbage) (char ',')

          garbage = Garbage <$> between (char '<') (char '>') garbageContent
          garbageContent = removed *> many ( noneOf ">" <* removed )

          removed = many ( char '!' >> anyChar )

day09 :: Group -> Int
day09 = recurse 1

recurse :: Int -> Group -> Int
recurse n (Garbage _) = 0
recurse n (Groups gs) = n + sum (map (recurse (n + 1)) gs)

day09bis :: Group -> Int
day09bis (Garbage s) = length s
day09bis (Groups gs) = sum . map day09bis $ gs

day09main :: IO ()
day09main = withInput "input09.txt" river >>= \input -> do
    putStrLn $ printf "Day 09 -- %d -- %d" (day09 input) (day09bis input)


-- day 10

makeHash256 :: [Int] -> [Int]
makeHash256 = flip map [0..255] . hashFun
    where
        -- foldl, with an extra, discarded accumulator
        foldlAcc f z acc = fst . foldl f (z, acc)
        hashFun = foldlAcc applySwap id (0, 0)
        applySwap (f, (pos, skip)) len = (f . swap pos len, (pos', skip + 1))
            where pos' = (pos + len + skip) % 256

swap :: Int -> Int -> Int -> Int
swap pos 0    n = n
swap pos span n = if keep n then n else (pos + span - 1 - (n - pos)) % 256
    where 
        keep x = not $ inRange x || inRange (x + 256)
        inRange x = pos <= x && x < (pos + span)

intList :: Parser [Int]
intList = sepBy1 number (char ',') <* newline

asciiLine :: Parser [Int]
asciiLine = many1 (ord <$> (noneOf "\n")) <* newline

day10 :: [Int] -> Int
day10 = foldl1 (*) . take 2 . makeHash256

day10bis :: [Int] -> String
day10bis spaces = printHash . densify . makeHash256 $ spaces'
    where
        spaces' = concat . replicate 64 $ spaces ++ [17, 31, 73, 47, 23]
        densify = map (foldl1 xor) . chunksOf 16 
        printHash = concatMap (printf "%02x")

day10main :: IO ()
day10main = withInput2 "input10.txt" intList asciiLine >>= \(lengths, charcodes) -> do
    putStrLn $ printf "Day 10 -- %d -- %s" (day10 lengths) (day10bis charcodes)


-- day 11

type Vec = [Int]

hexmove :: Parser Vec
hexmove = do dir <- liftM2 (\x y -> [x, y]) (oneOf "ns") (option ' ' (oneOf "we"))
             return $ case dir of
                "n " -> [ 1, -1,  0]
                "nw" -> [ 1,  0, -1]
                "sw" -> [ 0,  1, -1]
                "s " -> [-1,  1,  0]
                "se" -> [-1,  0,  1]
                "ne" -> [ 0, -1,  1]

moves :: Parser [Vec]
moves = sepBy1 hexmove (char ',') <* newline

distance :: Vec -> Int
distance = (`div` 2) . sum . map abs

day11 :: [Vec] -> Int
day11 = distance . foldl1 (zipWith (+))

day11bis :: [Vec] -> Int
day11bis = maximum . map distance . scanl1 (zipWith (+))

day11main :: IO ()
day11main = withInput "input11.txt" moves >>= \input -> do
    putStrLn $ printf "Day 11 -- %d -- %d" (day11 input) (day11bis input)


-- day 12


day12 :: a -> Int
day12 _ = 0

day12bis :: a -> Int
day12bis _ = 0

day12main :: IO ()
day12main = withInput "input11.txt" moves >>= \input -> do
    putStrLn $ printf "Day 12 -- %d -- %d" (day12 input) (day12bis input)
