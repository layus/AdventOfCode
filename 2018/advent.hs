
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M
import qualified Data.Matrix        as Mat
import qualified Data.Sequence      as Seq
import qualified Data.Set           as S
import qualified Data.UnionFind.ST  as UF

import Data.List        hiding (count)
import Data.Maybe
import Linear.V3
import Linear.V4

-- import Control.Applicative          (liftA2)
import Control.Arrow                ((&&&), (***), first, second, )
import Control.Exception            (catch)
import Control.Lens                 ((^?), ix, (&), _1, (^.), (.~))
import Control.Monad                (liftM2, msum, join, when)
import Control.Monad.Reader         (ReaderT, runReaderT, local, ask)
import Control.Monad.State.Strict   (State, execState, modify, get)
import Control.Monad.Zip            (MonadZip, mzip, mzipWith)
import Data.Bits                    ((.|.), (.&.))
import Data.Bool                    (bool)
import Data.Char                    (ord, isUpper, isLower, toLower)
import Data.Complex.Generic         (Complex((:+)), )
import Data.Data                    (Data, toConstr)
import Data.Either                  (partitionEithers)
import Data.Foldable                (fold, asum)
import Data.Function                (on)
import Data.Function.ArrayMemoize   (arrayMemo)
import Data.List.Unique             (repeated, repeatedBy, count_)
import Data.Map.Strict              (Map)
import Data.Matrix                  (Matrix, (!))
import Data.Modular                 (type (/)(), ℤ)
import Data.Ord                     (Down(Down))
import Data.SBV                     (Goal, SInteger, OptimizeStyle(Lexicographic), )
import Data.SBV                     (optimize, maximize, minimize, literal, sInteger, ite, (.<=), (.<), )
import Data.Sequence                ((<|), (|>), Seq((:<|), (:|>)))
import Data.Set                     (Set)
import Data.String                  (unlines)
import Data.Time.Clock              (getCurrentTime, diffUTCTime)
import Data.Tree                    (Tree(Node), foldTree)
import Data.Tuple                   (fst, snd, swap)
import Data.Typeable                (Typeable)
-- import Debug.Trace                  (traceShowId, traceShow, traceM, trace)
import GHC.ST                       (runST)
import System.IO.Error              (isDoesNotExistError)
import Text.Parsec                  ( many, many1, sepBy1, count, (<|>) -- repeats
                                    , char, string, noneOf, oneOf, lower, upper, letter, newline -- chars 
                                    , try, eof, optional, option, getPosition -- misc
                                    )
import Text.Parsec.Pos              (sourceLine, sourceColumn)
import Text.Parsec.String           (Parser, parseFromFile)
import Text.Parsec.Number           (int, nat)
import Text.Printf                  (printf)

-- infixl 7 % -- modulus has same precedence as (*) or (/)
-- (%) = mod


-- Parsers

getInput :: FilePath -> Parser a -> IO a
getInput path p = do
    input <- parseFromFile (p <* eof) path 
    either (error . show) return input

-- getInput2 :: FilePath -> Parser a -> Parser b -> IO (a, b)
-- getInput2 path p1 p2 = do
--     liftM2 (,) (getInput path p1) (getInput path p2)

-- tabMatrix :: Parser [[Int]]
-- tabMatrix = many1 (sepBy1 int (char '\t') <* newline)

parseLines :: Parser a -> Parser [a]
parseLines p = many1 (p <* newline)

-- Read

-- readInputFile :: Int -> IO String
-- readInputFile = readFile . printf "input%02d.txt"
-- 
-- readLine :: Int -> IO String
-- readLine i = head . lines <$> readInputFile i

class Display a where
    display :: a -> String

instance {-# OVERLAPS #-} Display String where
    display = id

instance Show a => Display a where
    display = show

day :: (Display b, Display c) => Int -> Parser a -> (a -> b) -> (a -> c) -> IO ()
day n parser p p' = flip catch ignore $ do
    start <- getCurrentTime
    input <- getInput (printf "input%02d.txt" n) parser
    let res1 = display $ p input
    mid <- res1 `seq` getCurrentTime
    let res2 = display $ p' input
    stop <- res2 `seq` getCurrentTime 
    let diff1 = (realToFrac $ diffUTCTime mid start) :: Double
    let diff2 = (realToFrac $ diffUTCTime stop mid) :: Double
    putStrLn $ printf "Day %02d -- %s (%.3f s) -- %s (%.3f s)" n res1 diff1 res2 diff2
  where ignore e | isDoesNotExistError e = return () 
                 | otherwise = putStrLn $ show e

main = do
    putStrLn "Start ..."
    day 25 day25parser day25 day25bis
    day 24 day24parser day24 day24bis
    day 23 day23parser day23 (const "Z3")
    day23bis =<< getInput "input23.txt" day23parser
    day 22 day22parser day22 day22bis
    day 21 day21parser day21 day21bis
    day 20 day20parser day20 day20bis
    day 19 day19parser day19 day19bis
    day 18 day18parser day18 day18bis
    day 17 day17parser day17 day17bis
    day 16 day16parser day16 day16bis
    day 15 day15parser day15 day15bis
    day 14 day14parser day14 day14bis
    day 13 day13parser day13 day13bis
    day 12 day12parser day12 day12bis
    day 11 day11parser day11 day11bis
    day 10 day10parser day10 day10bis
    day 9  day09parser day09 day09bis
    day 8  day08parser day08 day08bis
    day 7  day07parser day07 day07bis
    day 6  day06parser day06 day06bis
    day 5  day05parser day05 day05bis
    day 4  day04parser day04 day04bis
    day 3  day03parser day03 day03bis
    day 2  day02parser day02 day02bis
    day 1  day01parser day01 day01bis

hidden = const "xxx"

-- Day 01

day01parser :: Parser [Int]
day01parser = parseLines int

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

day02parser :: Parser [String]
day02parser = parseLines (many1 lower)

day02 :: [String] -> Int
day02 = uncurry (*) . (has 3 &&& has 2)
  where has x = sum . map (min 1 . length . repeatedBy (== x))

day02bis :: [String] -> String
day02bis = snd . firstDup . concatMap (zip [0..] . delOne [])
  where delOne :: String -> String -> [String]
        delOne prefix (x:xs) = (prefix ++ xs) : delOne (prefix ++ [x]) xs
        delOne prefix [] = []


-- Day 03

untilNat :: Parser Int
untilNat = try $ many (noneOf "0123456789\n") *> nat

day03parser :: Parser [[Int]]
day03parser = parseLines (many1 untilNat)

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

groupFstWith :: Ord a => ([b] -> c) -> [(a, b)] -> [(a, c)]
-- groupFstWith fold = map (head *** fold) . map unzip . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
groupFstWith fold = map (second fold) . M.toAscList . M.fromListWith (++) . map (second (:[]))

type Naps = [(Int, Int)]
type Shifts = [(Int, Naps)]
day04parser :: Parser Shifts
day04parser = filter (not . null . snd) . groupFstWith concat <$> many shift
  where shift = (,) <$> guard <*> many ((,) <$> try start <*> stop)
        guard = lastNatThen " begins shift"
        start = lastNatThen "] falls asleep"
        stop  = lastNatThen "] wakes up"
        lastNatThen s = last <$> many1 untilNat <* string s <* newline

maximumOn :: (Functor t, Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = snd . maximumBy (compare `on` fst) . adjoinFst f

mostCommon :: Ord a => [a] -> a
mostCommon = fst . last . count_

day04 :: Shifts -> Int
day04 = uncurry (*) . (id *** mostCommon . minutes) . maximumOn lazyness
  where
    minutes = concatMap (\(x, y) -> [x..y-1])
    lazyness = sum . map (uncurry subtract) . snd

day04bis :: Shifts -> Int
day04bis = uncurry (*) . (id *** snd) . maximumOn snd . map (id *** countSleeps)
  where
    minutes = concatMap (\(m, m') -> [m..m'-1])
    -- Naps -> (maxCount, mostFreqMinute)
    countSleeps = swap . head . count_ . minutes


-- Day 05

day05parser :: Parser [Int]
day05parser = many (toInt <$> letter) <* newline
  where toInt l | isUpper l = (ord 'A' - (ord l + 1))
                | isLower l = ((ord l + 1) - ord 'a')

day05 = length . foldr reduce []
  where reduce x (y:ys) = if x+y == 0 then ys else x:y:ys
        reduce x [] = [x]

day05bis = minimum . map day05 . zipWith removeUnit [1..26] . repeat
  where removeUnit i = filter (not . (== i) . abs)


-- Day 06

minimumsBy :: (a -> a -> Ordering) -> [a] -> [a]
minimumsBy compare = reverse . foldl mins []
  where mins [] x = [x]
        mins acc@(a:_) x = case compare a x of
                                GT -> [x]
                                EQ -> (x:acc)
                                LT -> acc

minimumsOn :: Ord b => (a -> b) -> [a] -> [a]
minimumsOn = minimumsBy . (compare `on`)

type Pt = (Int, Int) 
manhathan :: Pt -> Pt -> Int
manhathan (a, b) (c, d) = abs (c-a) + abs (d-b)

day06parser :: Parser [Pt]
day06parser = parseLines ((,) <$> int <* string ", " <*> int)

day06 :: [Pt] -> Int
day06 positions = maximum . map (length . snd) . noInfinite . groupFstWith id . adjoinClosestId $ grid
  where
    grid = [(x, y) | x <- [-150..450], y <- [-150..450], 300 >= manhathan (150, 150) (x, y)]
    border pt = manhathan (150, 150) pt == 300
    noInfinite = filter (not . any border . snd)
    adjoinClosestId = mapMaybe (\x -> (,x) <$> closest x)
    closest pt = case minimumsOn (manhathan pt . snd) . zip [1..] $ positions of
                                [(id, _)] -> Just id
                                _ -> Nothing

day06bis :: [Pt] -> Int
day06bis pos = length . filter (< 10000) . map (sum . flip map pos . manhathan) $ grid
    where grid = [(x, y) | x <- [-150..450], y <- [-150..450], 300 >= manhathan (150, 150) (x, y)]


-- Day 07

untilUpper :: Parser Char
untilUpper = many (noneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") *> upper

{- Deplist helpers -}
type DepList = [(Char, [Char])]
toDependencyList :: [[Char]] -> DepList
toDependencyList = groupFstWith concat . concatMap (\[a, b] -> [(b, [a]), (a, [])]) 
removeTask :: (Char, a) -> DepList -> DepList
removeTask (x, _) = filter ((/= x) . fst)
finishTask :: (Char, a) -> DepList -> DepList
finishTask (x, _) = map (second $ filter (/= x))

day07parser :: Parser DepList
day07parser = toDependencyList <$> parseLines line
    where line = char 'S' *> count 2 untilUpper <* many (noneOf "\n")

day07 :: DepList -> String
day07 = unfoldr getFirst
  where
    remove x = finishTask x . removeTask x
    getFirst items = (\task -> (fst task, remove task items)) <$> find (null . snd) items

day07bis :: DepList -> Int
day07bis = sum . unfoldr tasks . ([('0',0)], )
  where
    duration = (+61) . subtract (ord 'A') . ord
    toWorker (t, _) = (t, duration t)
    stopWhen = fromBool . not
    tasks (oldWorkers, oldTasks) = stopWhen (null oldWorkers) (jump, (newWorkers, newTasks))
      where
        -- Update workers
        jump = minimum $ map snd oldWorkers                         -- compute largest possible progress
        workers = map (second (subtract jump)) oldWorkers           -- Make progress for each worker
        (finished, remaining) = partition ((== 0) . snd) workers    -- Collect finished tasks
        -- Update tasks
        todoTasks = foldr finishTask oldTasks finished              -- Remove finished tasks from todo list.
        runnableT = filter (null . snd) todoTasks                   -- List available tasks
        -- start as many jobs as possible
        newWorkers = take 5 $ (remaining ++ map toWorker runnableT) -- run max 5 actions from available ones
        newTasks = foldr removeTask todoTasks newWorkers            -- Remove active tasks from todo list


-- Day 08

day08parser :: Parser (Tree [Int])
day08parser = do
    [nChild, nData] <- count 2 untilNat
    flip Node <$> count nChild day08parser <*> count nData untilNat

day08 = foldr ((+) . sum) 0
day08bis = foldTree getValue where
    getValue v [] = sum v
    getValue idx children = sum $ mapMaybe (\i -> children ^? ix (i-1)) idx

{- 
-- | A copy of Guillaume's algorithm
day08parser :: Parser (Int, Int)
day08parser = do
    [nb_children, nb_metadata] <- count 2 untilNat
    (childs, metadata) <- (,) <$> count nb_children day08parser <*> count nb_metadata untilNat
    return $ (sum metadata + (sum $ map fst childs), sum $ if null childs then metadata else [snd $ childs !! (i-1) | i <- metadata, 0 < i && i <= nb_children])
day08 = fst
day08bis = snd
-}


-- Day 09

justNat = untilNat <* many (noneOf "0123456789\n")

day09parser = (,) <$> justNat <*> justNat

shift :: Seq a -> Seq a
shift (xs :|> x) = x <| xs

unshift :: Seq a -> Seq a
unshift (x :<| xs) = xs |> x

game :: (Int, Int) -> (IM.IntMap Int, Seq Int, Int)
game (players, steps) = iterate' (rules players) init !! steps
  where init = (IM.empty, Seq.fromList [0], 1)
rules :: Int -> (IM.IntMap Int, Seq Int, Int) -> (IM.IntMap Int, Seq Int, Int)
rules players (!score, !circle, !marble)
    | marble `mod` 23 == 0 = let x :<| xs = iterate' unshift circle !! 7 in
            (IM.insertWith (+) (marble `mod` players) (marble + x) score, shift xs, marble + 1) 
    | otherwise = (score, marble <| (shift circle), marble + 1)

day09 = maximum . (^. _1) . game
day09bis = day09 . (second (* 100))


-- Day 10

-- aligned (((x, y), _):ps) = (length $ filter (\((a, b), _) -> a == x || b == y) ps) > 40
justInt = skip *> int <* skip 
  where skip = many (noneOf "+-0123456789\n")

day10parser = parseLines $ format <$> count 4 justInt
  where format [x, y, dx, dy] = ((x + 10000 * dx, y + 10000 * dy), (dx, dy))

move ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))
aligned' = (== 9) . uncurry subtract . snd . bounds -- Merci Guillaume ;-)
bounds = join (***) (minimum &&& maximum) . unzip . map fst 

showStars ps = unlines $ "":[ [ if (x, y) `elem` pos then '▓' else '░'
                              | x <- [minX..maxX] ]
                            | y <- [minY..maxY] ]
  where pos = S.fromList (map fst ps)
        ((minX, maxX), (minY, maxY)) = bounds ps

day10 = showStars . until aligned' (map move)
day10bis = (10000 +) . length . takeWhile (not . aligned') . iterate (map move)


-- Day 11

adjoinFst :: Functor m => (a -> b) -> m a -> m (b, a)
adjoinFst = fmap . (\f x -> let y = f x in y `seq` (y, x))


type Pt3 = (Int, Int, Int)
memoizePt :: (Int, Int) -> (Pt -> Int) -> (Pt -> Int)
memoizePt (x, y) f = (lookupTable !) where lookupTable = Mat.matrix x y f

day11parser = power . cumsum (301, 301) . cell <$> justNat <* newline
  where
    cell serial (x, y) = (rack * y + serial) * rack `div` 100 `mod` 10 - 5 where rack = x + 10
    cumsum bounds cell = calc
      where calc = memoizePt bounds calc'
            calc' (1, _) = 0
            calc' (_, 1) = 0
            calc' (i, j) = cell (i-1, j-1) + calc (i-1, j) + calc (i, j-1) - calc (i-1, j-1)
    power cumsum (i, j, n) = cumsum (i+n, j+n) - cumsum (i+n, j) - cumsum (i, j+n) + cumsum (i, j)


day11 :: (Pt3 -> Int) -> Pt
day11 power = maximumOn (power . uncurry (,,3)) $ [ (i, j) | i <- [1..298], j <- [1..298] ]

day11bis :: (Pt3 -> Int) -> Pt3
day11bis power = maximumOn power $ [ (i, j, n) | n <- [1..300], i <- [1..301-n], j <- [1..301-n] ]


-- Day 12

windows :: Int -> [a] -> [[a]]
windows n l@(_:t) = if length win < n then [] else win:(windows n t)
  where win = take n l

day12parser :: Parser (Int -> Int)
day12parser = potsGame <$> state <*> rules
  where state = string "initial state: " *> plants <* many newline
        rules = (M.!) . M.fromList <$> parseLines ((,) <$> plants <* string " => " <*> plant)
        plant = oneOf "#."
        plants = many1 plant

potsGame :: String -> (String -> Char) -> Int -> Int
potsGame init rules = evalPots . game 0 init
  where 
    evalPots (offset, pots) = sum . map (+offset) . findIndices (== '#') $ pots
    normalize pots = let (stem, pots') = span (== '.') pots 
                     in (length stem - 2, dropWhileEnd (== '.') pots')
    patterns = windows 5 . ("...." ++) . (++ "....")
    game offset pots steps
        | steps == 0    = (offset,                       pots)
        | pots == pots' = (offset + extraOffset * steps, pots)
        | otherwise     = game (offset + extraOffset) pots' (steps - 1)
        where (extraOffset, pots') = normalize . map rules . patterns $ pots

day12 :: (Int -> Int) -> Int
day12 = ($ 20)

day12bis :: (Int -> Int) -> Int
day12bis = ($ 50000000000) 


-- Day 13

data Train = Train { position :: Complex Int, direction :: Complex Int, choice :: ℤ / 3}
    deriving (Show)

instance Ord (Complex Int) where
    compare (a :+ b) (x :+ y) = compare (a, b) (x, y)

instance Ord Train where
    compare = compare `on` position

instance Eq Train where
    (==) = (==) `on` position

day13parser :: Parser (Complex Int -> Char, [Train])
day13parser = (tracks &&& trains) . Mat.fromLists <$> parseLines (many $ noneOf "\n")
  where 
    dir = (M.!) . M.fromList $ zip "><^v" [1:+0, (-1):+0, 0:+(-1), 0:+1]
    tracks = (\m (y:+x) -> m Mat.! (x+1, y+1))
    trains = fold . Mat.mapPos (\(y, x) v -> if v `elem` "><^v" then [ Train ((x-1):+(y-1)) (dir v) 0 ] else [])

runTrains (tracks, trains) = collide [] trains
  where
    collide :: [Train] -> [Train] -> [Train]
    collide [] []    = []
    collide [] [t]    = [t]
    collide [] ts     = collide (sort ts) []
    collide (t:ts) ms = case o of
        Just o' -> t':o':collide ts' ms'
        Nothing -> collide ts (t':ms)
      where 
        t' = moveTrain t
        o = find (== t') $ ts ++ ms
        ts' = filter (/= t') ts
        ms' = filter (/= t') ms

    moveTrain :: Train -> Train
    moveTrain (Train pos v c) = case (tracks pos', v) of
        ('+' , _   ) -> Train pos' (v * rot c) (c+1)
        ('\\', 0:+_) -> Train pos' (v * left) c
        ('\\', _   ) -> Train pos' (v * right) c
        ('/' , 0:+_) -> Train pos' (v * right) c
        ('/' , _   ) -> Train pos' (v * left) c
        _            -> Train pos' v c
      where pos' = pos + v
            rot c = genericIndex [left, (1 :+ 0), right] c
            left = 0 :+ (-1)
            right = 0 :+ 1

day13 = (\(x:+y) -> (x, y)) . position . head . runTrains
day13bis = (\(x:+y) -> (x, y)) . position . last . runTrains


-- Day 14

day14parser = int <* newline

digits :: Int -> [Int]
digits = map (read . pure) . show 

generate :: [Int]
generate = 3 : 7 : go 0 1 (Seq.fromList [3, 7])
  where
    go !i !j !s = extra ++ go i' j' s'
      where
        extra = digits (vi + vj)
        vi = s `Seq.index` i
        vj = s `Seq.index` j
        i' = (i + 1 + vi) `mod` (length s')
        j' = (j + 1 + vj) `mod` (length s')
        s' = s <> Seq.fromList extra

day14 = concatMap show . take 10 . flip drop generate
day14bis :: Int -> Int
day14bis n = 20283721 -- (too slow!) -- length $ takeWhile (not . (xs `isPrefixOf`)) (tails generate)
  where xs = digits n


-- Day 15

-- Heavily inspired / copied from 'https://github.com/ephemient/aoc2018/blob/master/src/Day15.hs'

data Cave = Cave {walls :: Set Pt, units :: Map Pt Unit} deriving (Show)
data Species = Goblin | Elf deriving (Show, Ord, Eq)
data Unit = Unit { species :: Species, hp :: Int } deriving (Show)

day15parser :: Parser Cave
day15parser = uncurry Cave . (S.fromDistinctAscList *** M.fromDistinctAscList) . partitionEithers <$> many element
  where element = do
            -- index by (y, x) to get correct order.
            pos <- (sourceLine &&& sourceColumn) <$> getPosition
            c <- oneOf "#EG." <* many (oneOf ".\n")
            return $ case c of
                          '#' -> Left pos
                          'E' -> Right (pos, Unit Elf 200)
                          'G' -> Right (pos, Unit Goblin 200)

adjacencies :: Pt -> Set Pt
adjacencies (y, x) = S.fromDistinctAscList [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

nearest :: Set Pt -> Set Pt -> Pt -> Maybe Pt
nearest walls goals = go walls . S.singleton
  where
    go visited q 
        | S.null q = Nothing
        | reached <- S.intersection goals q, not $ S.null reached = S.lookupMin reached
        | otherwise = go visited' $ S.unions (adjacencies <$> S.toList q) S.\\ visited'
        where visited' = S.union visited q

step :: Int -> Set Pt -> Map Pt Unit -> (Map Pt Unit, Bool)
step force walls = go M.empty
  where
    go past (M.minViewWithKey -> Just ((pos, unit), future))
        | M.null enemies = (M.insert pos unit allOtherUnits, False)
        | otherwise = case M.restrictKeys enemies $ adjacencies pos' of
            (sortOn (hp . snd) . M.toList -> (target, _):_) -> let
                past' = M.insert pos' unit $ M.alter attack target past 
                future' = M.alter attack target future
              in go past' future'
            _ -> go (M.insert pos' unit past) future
        where 
            allOtherUnits = M.union past future
            walls' = S.union walls $ M.keysSet allOtherUnits
            enemies = M.filter (on (/=) species $ unit) allOtherUnits
            adjacentEnemies = adjacencies pos `S.intersection` M.keysSet enemies
            enemyRanges = S.unions (adjacencies <$> M.keys enemies) S.\\ walls'
            pos' = case if S.null adjacentEnemies then nearest walls' enemyRanges pos else Nothing of
                        Just goal | Just move <- nearest walls' (adjacencies pos) goal -> move
                        _ -> pos
    go past _ = (past, True)

    attack (Just unit@Unit {species = Elf, hp})
        | hp > 3 = Just unit {hp = hp - 3}
    attack (Just unit@Unit {species = Goblin, hp})
        | hp > force = Just unit {hp = hp - force}
    attack _ = Nothing

outcome :: Cave -> (Int, Int)
outcome Cave {..} = outcome' 0 units
  where outcome' rounds units = case step 3 walls units of
            (units', False) -> (rounds, sum $ map hp $ M.elems units')
            (units', True) -> outcome' (rounds + 1) units'

outcomeWith :: Cave -> Int -> Maybe (Int, Int)
outcomeWith Cave {..} i = outcome' 0 units
  where outcome' rounds units' = case step i walls units' of
            (units'', False) -> fromBool (allAlive units'') (rounds, sum $ map hp $ M.elems units'')
            (units'', True) -> outcome' (rounds + 1) units''
        countElves = M.size . M.filter ((== Elf) . species)
        allAlive = (== countElves units) . countElves
    
day15 = uncurry (*) . outcome
day15bis cave = uncurry (*) . head . mapMaybe (outcomeWith cave) $ [4..]


-- Day 16

type Registers = [Int]
data Instr = Instr { opcode :: Int, a :: Int, b :: Int, to :: Int } deriving (Eq)

instance Show Instr where
    show (Instr o a b c) = printf "%d %d %d %d" o a b c

data Op = Addr | Addi
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
  deriving (Ord, Eq, Bounded, Enum, Data, Typeable)

instance Show Op where
    show = map toLower . show . toConstr

apply :: Op -> Instr -> Registers -> Registers
apply o (Instr _ a b to) input = input & ix to .~ case o of
    Addr -> reg (+)
    Addi -> imm (+)
    Mulr -> reg (*)
    Muli -> imm (*)
    Banr -> reg (.&.)
    Bani -> imm (.&.)
    Borr -> reg (.|.)
    Bori -> imm (.|.)
    Setr -> imm (const) -- !
    Seti -> inv (const) -- !
    Gtri -> imm ((boolint .) . (>))
    Gtir -> inv ((boolint .) . (>))
    Gtrr -> reg ((boolint .) . (>))
    Eqri -> imm ((boolint .) . (==))
    Eqir -> inv ((boolint .) . (==))
    Eqrr -> reg ((boolint .) . (==))
  where
    reg op = (input !! a) `op` (input !! b)
    imm op = (input !! a) `op` (         b)
    inv op = (         a) `op` (input !! b)
    boolint = bool 0 1 

data Test = Test { input :: Registers, instr :: Instr, result :: Registers } deriving (Eq)
instance Show Test where
    show (Test i d o) = printf "\n%s\n%s\n%s\n" (show i) (show d) (show o)

day16parser = (,) <$> tests <* newline <* newline <*> program
  where tests = many (try test)
        test = Test <$> four <*> (toInstr <$> four) <*> four <* newline
        program = many (toInstr <$> four)
        toInstr [a, b, c, d] = Instr a b c d
        four = count 4 justInt <* newline

check (Test inp ins out) op = out == apply op ins inp
choices = [Addr ..]

day16 :: ([Test], [Instr]) -> Int
day16 = length . filter ambiguous . fst
  where ambiguous test = (>= 3) . length . filter (check test) $ choices

day16bis (tests, prgm) = head $ foldl ops [0, 0, 0, 0] prgm
  where
    finalOps = M.map (\[a] -> a) $ (!! 16) $ iterate filterUniques $ M.fromListWith intersect $ map valid tests
    valid test = (opcode . instr $ test, filter (check test) choices)
    ops reg instr = apply (finalOps M.! opcode instr) instr reg
    filterUniques options = M.map (\v -> case v of [_] -> v; _ -> v \\ uniques) options
      where uniques = concat . filter ((== 1) . length) . snd . unzip $ M.toList options



-- Day 17

-- | Binary operators to chain monads on Booleans
-- | These operators are fail-fast (except <&&>)
infixl 1 <&&>, ||>, &&>
(&&>), (||>), (<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
a  &&> b = a >>= bool (return False) b
a  ||> b = a >>= bool b (return True)
a <&&> b = liftM2 (&&) a b

type Grid = Map Pt Char

day17parser :: Parser Grid
day17parser = flow . M.fromList . map (,'#') . concat <$> parseLines line where 
    line = do 
        axis <- char 'x' <|> char 'y'
        [a, b, c] <- count 3 justNat
        let coord = if axis == 'x' then (a,) else (,a)
        return $ map coord (enumFromTo b c)

showRocks :: Grid -> String
showRocks grid = unlines [ [ M.findWithDefault ' ' (x, y) grid 
                           | x <- [minX .. maxX] ] 
                         | y <- [minY .. maxY] ]
  where ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid

-- A flow returns a boolean to tell whether it overflowed.
type Flow = ReaderT Pt (State Grid) Bool
type Dir = Pt -> Pt

flow :: Grid -> Grid
flow grid = execState (runReaderT fillDown (500, minY)) grid
  where 
    ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
    (left, right, down) = (first (subtract 1), first (+1), second (+1))

    -- Performs a given action when the cell is empty
    -- but return a conforming boolean if it is not.
    onFree :: Flow -> Flow
    onFree action = ask >>= \(x, y) ->
        if y > maxY || x > maxX+10 || x < minX-10
        then return False -- border never overflows
        else M.lookup (x, y) <$> get >>= \case
                Just '|' -> return False -- no overflow
                Just _   -> return True  -- overflow ('~', '#')
                Nothing  -> action

    -- Set current cell to @c@
    set c = ask >>= \pos -> modify (M.insert pos c)
    -- Small helper to move and test current cell altogether
    go dir = local dir . onFree
    -- Nice wording ;-)
    onOverflow = (&&>)

    fillDown :: Flow
    fillDown = set '|' >> go down (fillDown
             `onOverflow` (fill left <&&> fill right)
             `onOverflow` (markStill left <&&> markStill right))

    -- We need to fill further left (resp. right) if the tile below us overflows 
    fill dir = go dir $ fillDown `onOverflow` fill dir

    -- Fill with still water until we reach an already filled tile. 
    markStill dir = set '~' >> local dir (isFilled ||> markStill dir) where
        -- cell cannot be free, arg is irrelevant.
        isFilled = onFree undefined


day17 = length . filter (/= '#') . M.elems
day17bis = length . filter (== '~') . M.elems


-- Day 18

type Forest = Matrix Char

day18parser :: Parser Forest
day18parser = Mat.fromLists <$> parseLines (many $ oneOf "#|.")

countEach :: [Char] -> [Int]
countEach = (\m -> map (\c -> M.findWithDefault 0 c m) "|#") . M.fromList . count_

collectWood :: Forest -> Forest
collectWood forest = Mat.mapPos (magic . context) forest
  where
    magic [t, l] '.' = bool '.' '|' (t >= 3)
    magic [t, l] '|' = bool '|' '#' (l >= 3)
    magic [t, l] '#' = bool '#' '.' (l < 1 || t < 1)
    (nrows, ncols) = (Mat.nrows &&& Mat.ncols) forest
    context (x, y) = countEach [ forest Mat.! (x', y') 
                               | x' <- [max 1 (x-1) .. min ncols (x+1)],
                                 y' <- [max 1 (y-1) .. min nrows (y+1)],
                                 (x, y) /= (x', y')]

-- Make Forest usable for Map keys
instance Ord Forest where compare = compare `on` Mat.toLists

fixLoop :: Ord a => (a -> a) -> Int -> a -> a
fixLoop f = fixLoop' M.empty where 
  fixLoop' visited i s = if i == 0 then s else case M.lookup s visited of
    Just i' -> head $ M.keys $ M.filter (== i' - (i `mod` (i'-i))) visited
    Nothing -> fixLoop' (M.insert s i visited) (i-1) (f s)

showForest = unlines . Mat.toLists
evalForest = (\[t, l] -> l * t) . countEach . Mat.toList 
day18 = evalForest . fixLoop collectWood 10
day18bis = evalForest . fixLoop collectWood 1000000000


-- Day 19

parseRepr :: Data a => a -> Parser a
parseRepr i = const i <$> string (map toLower . show . toConstr $ i)

data Program = Program Int (Seq Instr')
data Instr' = Instr' Op Int Int Int 

day19parser = Program <$> bind <*> (Seq.fromList <$> parseLines instr)
  where
    bind = string "#ip " *> justNat <* newline
    instr = Instr' <$> opcode <*> justNat <*> justNat <*> justNat
    opcode = asum $ map (try . parseRepr) [Addr ..]

apply' :: Instr' -> Registers -> Registers
apply' (Instr' op a b to) = apply op (Instr 0 a b to)

isqrt = floor . sqrt . fromIntegral
factors x = concat [ [y, x `div` y] | y <- [1..isqrt x], x `mod` y == 0]

exec i0 = sum . factors . (!!5) . (!!100) . flip iterate [i0, 0, 0, 0, 0, 0] . execLine
execLine (Program ipReg instrs) regs = incIp $ flip apply' regs $ (Seq.index instrs (regs !! ipReg))
    where incIp = apply' (Instr' Addi ipReg 1 ipReg)

day19 = exec 0
day19bis = exec 1


-- Day 20

data Regex = Chain [Regex] | Alt [Regex] | Exact Char deriving (Show, Eq)

day20parser :: Parser Regex
day20parser = char '^' *> regex <* string "$\n"
  where
    regex = Chain <$> many (text <|> choice)
    choice = Alt <$> ((char '(') *> (sepBy1 regex (char '|')) <* (char ')'))
    text = Exact <$> oneOf "NSWE"

type Adjacencies = Map Pt (Set Pt)

walk :: Regex -> Adjacencies
walk = fst . go (M.empty, S.singleton (0, 0))
  where
    go (adj, pos) (Chain rs) = foldl go (adj, pos) rs
    go (adj, pos) (Alt rs) = (M.unionsWith S.union *** S.unions) $ unzip $ map (go (adj, pos)) rs
    go (adj, pos) (Exact c) = (adj', pos')
      where 
        pos' = S.map (move c) pos
        adj' = S.foldl insertBoth adj pos
        insertAdj p p' = M.insertWith S.union p (S.singleton p')
        insertBoth adj p = insertAdj p p' $ insertAdj p' p $ adj 
          where p' = move c p

    move 'N' = second $ subtract 1
    move 'S' = second $ (+1)
    move 'W' = first $ subtract 1
    move 'E' = first $ (+1)

dijkstra :: Int -> Adjacencies -> (Int, Set Pt)
dijkstra n adjMap = go 0 S.empty $ S.singleton (0, 0)
  where
    go s visited q
        | s == n || S.null q = (s, visited)
        | otherwise = go (s+1) visited' q'
        where 
          visited' = S.union visited q
          q' = S.unions ((adjMap M.!) <$> S.toList q) S.\\ visited'


farthest = subtract 1 . fst . dijkstra (-1)
roomsWithin n = snd . dijkstra n

day20 = farthest . walk
day20bis = uncurry subtract . (S.size . roomsWithin 1000 &&& M.size) . walk

-- debug

printBuilding :: Adjacencies -> String
printBuilding adj = unlines [ [ c i j
                              | i <- [inc (2*minX) .. inc (2*maxX)] ]
                            | j <- [inc (2*minY) .. inc (2*maxX)] ]
  where
    ((minX, maxX), (minY, maxY)) = bounds (M.toList adj)
    inc x = if x > 0 then x + 1 else x - 1
    c i j | i == 0 && j == 0 = 'X' -- start
          | even i && even j = if p' `M.member` adj then '.' else '#' -- room
          | i `quot` 2 < minX || i `quot` 2 > maxX = '#' -- border x
          | j `quot` 2 < minY || j `quot` 2 > maxY = '#' -- border y
          | even i && odd  j && p `S.member` (adj M.! p') = '-' -- door
          | odd  i && even j && p `S.member` (adj M.! p') = '|' -- door
          | otherwise = '#'
          where
            even v = v `mod` 2 == 0
            odd v = v `mod` 2 == 1
            p  = ((inc i) `quot` 2, (inc j) `quot` 2)
            p' = (i `quot` 2, j `quot` 2)


-- Day 21

hash :: Int -> Int -> [Int]
hash b e = if b >= 256 then hash b' e'
           else e' : hash (e' .|. 65536) 2024736
  where
    e' = (.&. 16777215) . (* 65899) . (.&. 16777215) . (+ (b .&. 255)) $ e
    b' = (b `div` 256)

takeWhileUnique :: Ord a => [a] -> [a]
takeWhileUnique = go S.empty where
    go s (x:xs) | x `S.member` s = []
                | otherwise = x : go (S.insert x s) xs

day21parser = const (hash 65536 2024736) <$> day19parser
day21 = head
day21bis = last . takeWhileUnique


-- Day 22

computeCave :: Int -> Pt -> (Pt -> Int, Pt)
computeCave depth (tx, ty) = ((`mod`3) . calc, (tx, ty))
  where 
    calc = arrayMemo ((0,0), (200*tx, 2*ty)) ((`mod` 20183) . (+ depth) . calc')
    calc' (0, 0) = 0
    calc' (x, 0) = (x * 16807)
    calc' (0, y) = (y * 48271)
    calc' (x, y) | (x, y) == (tx, ty) = 0
    calc' (x, y) = calc (x-1, y) * calc (x, y-1)
    shift = join (***) (+1)

speleology :: (Pt -> Int) -> Pt -> Int
speleology danger target = go 0 S.empty (M.singleton 0 (S.singleton (1, (0, 0))))
  where
    go n visited q
        | (1, target) `S.member` visited' = n
        | otherwise = go n' visited' q'
        where
          visited' = S.union visited visiting
          ((n', visiting), remaining) = M.deleteFindMin q
          q' = foldl (flip $ uncurry $ M.insertWith S.union) remaining neighbors
          neighbors = map (second S.singleton) $ filter (not . (`S.member` visited') . snd) $ concatMap next (S.toList visiting)
          next a = nextTool a ++ nextPos a
          nextTool (t, pos) = [ (n'+7, (t', pos)) | t' <- [0..2], t' /= t, t' /= danger pos ]
          nextPos (t, (x, y)) = [ (n'+1, (t, (x', y'))) | (x', y') <- adj, x' >= 0 && y' >= 0, t /= danger (x', y') ]
                                where adj = [(x-1, y), (x+1, y), (x, y+1), (x, y-1)]

day22parser = computeCave <$> justInt <* newline <*> ( (,) <$> justInt <*> justInt <* newline)
day22 (danger, (x, y)) = sum [ danger (x, y) | x <- [0..x], y <- [0..y] ]
day22bis (danger, target) = 1 + speleology danger target


-- Day 23

data Robot = Robot { pos :: V3 Int, range :: Int } 
day23parser = parseLines $ Robot <$> (V3 <$> justInt <*> justInt <*> justInt) <*> justInt

day23 robots = length $ filter (inRangeOf strongest) robots
  where 
    strongest = maximumOn range robots
    manhathan3 = sum .: mzipWith (abs .: subtract)
    inRangeOf (Robot p r) (Robot p' _) = r >= manhathan3 p p'
    inRange p r = range r >= manhathan3 p (pos r)


problem :: [Robot] -> Goal
problem robots = do
    pt <- mapM sInteger (V3 "x" "y" "z")
    maximize "nInRange" $ sum (map (inRange pt) robots)
    minimize "distance" $ manhathan3 pt (V3 0 0 0)
  where
    manhathan3 :: V3 SInteger -> V3 Int -> SInteger
    manhathan3 pt pos = foldl1 (+) $ fmap (abs . uncurry subtract) $ mzip pt (fmap lit pos)

    inRange :: V3 SInteger -> Robot -> SInteger
    inRange pt (Robot pos rad) = reify $ manhathan3 pt pos .<= lit rad

    abs x = ite (x .< 0) (-x) (x)
    reify b = ite b 1 0
    lit = literal . fromIntegral

day23bis robots = do
    start <- getCurrentTime
    opt <- show <$> optimize Lexicographic (problem robots)
    stop <- opt `seq` getCurrentTime
    let diff = (realToFrac $ diffUTCTime stop start) :: Double
    putStrLn $ printf "Day 23 -- %s (%.3f s)" opt diff


-- Day 24

data AttackKind = Fire | Bludgeoning | Cold | Radiation | Slashing
  deriving (Ord, Eq, Bounded, Enum, Data, Typeable, Show)
data Army = Immune | Infection
  deriving (Ord, Eq, Bounded, Enum, Data, Typeable, Show)

data Group = Group {
        nunits :: Int, hps :: Int, 
        weak :: Set AttackKind, immune :: Set AttackKind,
        attack :: Int, kind :: AttackKind, initiative :: Int,
        team :: Army, rank :: Int
    } deriving (Show, Eq)

day24parser = (++) <$> parseArmy Immune <* newline <*> parseArmy Infection
  where
    parseArmy :: Army -> Parser [Group]
    parseArmy army = zipWith (\i g -> g army i) [1..] <$> (many (noneOf "\n") *> newline *> groups)
    groups = parseLines group
    group :: Parser (Army -> Int -> Group)
    group = do                           -- Example:
        units <- untilNat                   -- "1432"
        hp <- untilNat                      -- " units each with 7061"
        string " hit points "               -- " hit points "
        optional (char '(')                 -- "("
        weak1 <- modifiers "weak to "       -- ""
        immune <- modifiers "immune to "    -- "immune to cold; "
        weak2 <- modifiers "weak to "       -- "weak to bludgeoning)"
        -- hack, hack, hack...
        let weak = S.union weak1 weak2
        attack <- untilNat                  -- " with an attack that does 41"
        kind <- char ' ' *> attackKind      -- " slashing"
        initiative <- untilNat              -- " damage at initiative 17"
        return $ Group units hp weak immune attack kind initiative 
    modifiers :: String -> Parser (Set AttackKind)
    modifiers name = option S.empty $ try $ string name *> attackKinds <* (string ")" <|> string "; ")
    attackKinds :: Parser (Set AttackKind)
    attackKinds = S.fromList <$> attackKind `sepBy1` string ", "
    attackKind :: Parser AttackKind
    attackKind = asum $ map (try . parseRepr) [Fire ..]

fight boost groups 
  | groups == groups' = groups
  | otherwise = fight boost groups'
  where
    groups' = filter ((> 0) . nunits) . doAttack . selectTargets $ groups

    effectivePower (Group {team = Immune, ..}) = nunits * (attack + boost)
    effectivePower (Group {..})                = nunits * attack

    dammage a b 
        | team a == team b = 0           -- same team, no attack.
        | kind a `S.member` immune b = 0
        | kind a `S.member` weak b = 2 * effectivePower a
        | otherwise = effectivePower a

    selectTargets = fst . foldl' selectTarget ([], groups) . sortOn selectOrder
      where
        selectOrder = Down . (effectivePower &&& initiative)
        selectTarget (attacks, defenders) attacker = ((attacker, target):attacks, remainers)
          where
            target = listToMaybe . sortOn targetOrder . filter ((> 0) . dammage attacker) $ defenders
            targetOrder = Down . (dammage attacker &&& effectivePower &&& initiative)
            remainers = maybe id (filter . (/=)) target $ defenders

    doAttack = M.elems . foldl' attack initialTeams . sortOn attackOrder
      where
        attackOrder = Down . initiative . fst
        initialTeams = M.fromList $ adjoinFst initiative groups
        attack teams (a, Nothing) = teams
        attack teams (a, Just d) = M.insert (initiative d') d' teams
          where realA = teams M.! initiative a
                realD = teams M.! initiative d
                d' = realD {nunits = max 0 $ (nunits realD) - (dammage realA realD `div` hps realD) }

day24 = sum . map nunits . fight 0
day24bis = sum . map nunits . head . dropWhile (not . win) . zipWith fight [1..] . repeat
  where win = all ((Immune ==) . team)


-- Day 25

day25parser :: Parser [V4 Int]
day25parser = parseLines (V4 <$> justInt <*> justInt <*> justInt <*> justInt)
  where 
    justInt = skip *> int <* skip 
    skip = many (noneOf "+-0123456789\n")

(.:) :: (x -> y) -> (a -> b -> x) -> (a -> b -> y)
(.:) = (.) . (.)

manhathan4 :: (MonadZip m, Foldable m, Num a) => m a -> m a -> a
manhathan4 = sum .: mzipWith (abs .: subtract)

day25 points = runST $ do 
    ps <- zip points <$> mapM UF.fresh points
    flip mapM ps $ \(a, pa) -> do
        flip mapM ps $ \(b, pb) -> do
            when (manhathan4 a b <= 3) $ UF.union pa pb
    redundant <- mapM (UF.redundant . snd) ps
    return $ length (filter not redundant)

day25bis = const "Finished"

