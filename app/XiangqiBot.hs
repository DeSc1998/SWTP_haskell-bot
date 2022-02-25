{-# LANGUAGE DeriveFunctor #-}

-- module (NICHT ÄNDERN!)
module XiangqiBot
  ( getMove,
    listMoves,
  )
where

-- More modules may be imported

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

-- import           Util

getMove :: String -> String
getMove strBoard = convertMove move
  where
    (board, player) = fst . fromJust $ run configP strBoard
    moves = availableMoves player board
    (_, moveHistory) = pickBest $ rateMoves 4 [moves] player [(board, [])]
    move = last moveHistory

listMoves :: String -> String
listMoves strBoard = "[" ++ concatMap convert moves ++ "]"
  where
    (board, player) = fst . fromJust $ run configP strBoard
    moves = availableMoves player board
    convert m = convertMove m ++ ","

-- assume board is already expanded
availableMoves :: Color -> Board -> [Move]
availableMoves player board = moves
  where
    listPiceMoves pos pice = (pice, movesOfPice pice pos)
    valid pice move = isValidMove pice move board
    filterValidMoves (pice, ms) = filter (valid pice) ms
    moves =
      concat
        . Map.elems
        . Map.map filterValidMoves
        $ Map.mapWithKey listPiceMoves $
          playerPices player board

data Color
  = Red
  | Black
  deriving (Show, Eq)

data Pice
  = None Int
  | Gene Color
  | Advi Color
  | Elep Color
  | Hors Color
  | Rook Color
  | Cano Color
  | Sold Color
  deriving (Show, Eq)

type Row = [Pice]

type BoardOld = [Row]

type Board = Map.Map Position Pice

type Position = (Int, Int)

type Move = (Position, Position)

newtype Parser a = Parser {run :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \y -> Just (x, y)
  p1 <*> p2 =
    Parser
      ( \input -> do
          (f, input') <- run p1 input
          (a, input'') <- run p2 input'
          Just (f a, input'')
      )

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Semigroup (Parser a) where
  p1 <> p2 = p1 <|> p2

instance Monoid (Parser a) where
  mempty = empty

piceColor :: Char -> Color
piceColor c
  | isUpper c = Red
  | isLower c = Black
  | otherwise = undefined

charToPice :: Char -> Pice
charToPice c
  | toUpper c == 'G' = Gene (piceColor c)
  | toUpper c == 'A' = Advi (piceColor c)
  | toUpper c == 'E' = Elep (piceColor c)
  | toUpper c == 'H' = Hors (piceColor c)
  | toUpper c == 'R' = Rook (piceColor c)
  | toUpper c == 'C' = Cano (piceColor c)
  | toUpper c == 'S' = Sold (piceColor c)
  | isDigit c = None (ord c - ord '0')
  | otherwise = undefined

convertPos :: Position -> String
convertPos (x, y) = [chr (x + ord 'a'), chr (y + ord '0')]

convertMove :: Move -> String
convertMove (start, end) = convertPos start ++ "-" ++ convertPos end

charPices :: String
charPices = "GAEHRCSgaehrcs123456789"

expandNone :: Pice -> [Pice]
expandNone (None n)
  | n > 1 = None 1 : expandNone (None (n - 1))
  | n == 1 = [None 1]
  | n < 1 = []
expandNone p = [p]

expand :: [Pice] -> [Pice]
expand = concatMap expandNone

-- Parser definitions
charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs)
      | x == c = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

pPice :: Char -> Parser Pice
pPice c = charToPice <$> charP c

piceP :: Parser Pice
piceP = foldMap pPice charPices

rowP :: Parser Row
rowP = many piceP

boardP :: Parser BoardOld
boardP = many (rowP <* (charP '/' <|> charP ' '))

playerP :: Parser Color
playerP = f <$> (charP 'r' <|> charP 'b')
  where
    f c
      | c == 'r' = Red
      | c == 'b' = Black
      | otherwise = undefined

configP :: Parser (Board, Color)
configP = f <$> boardP <*> playerP
  where
    indexedRow (x, y) (p : ps) =
      ((x, y), p) : indexedRow (x + 1, y) ps :: [(Position, Pice)]
    indexedRow _ [] = []
    indexed y (r : rs) =
      indexedRow (0, y) r : indexed (y + 1) rs :: [[(Position, Pice)]]
    indexed _ [] = []
    convert = filter (\(_, p) -> p /= None 1) . concat . indexed 0 . map expand . reverse
    f b p = (Map.fromList $ convert b, p)

-- end of Parser definitions

-- Move evaluation
piceValue :: Pice -> Int
piceValue (None _) = 0
piceValue (Gene _) = 1000
piceValue (Advi _) = 1
piceValue (Rook _) = 20
piceValue (Cano _) = 15
piceValue (Hors _) = 7
piceValue (Sold _) = 4
piceValue (Elep _) = 3

isPice :: Pice -> Bool
isPice (None _) = False
isPice _ = True

evaluateBoard :: Color -> Board -> Int
evaluateBoard c = Map.foldl (+) 0 . Map.map f . Map.filter isPice
  where
    f p
      | isOpponent p c = -(piceValue p)
      | otherwise = piceValue p

setAt :: Position -> Pice -> Board -> Board
setAt pos pice = Map.adjust f pos where f _ = pice

-- assume the move is correct
applyMove :: Move -> Board -> Board
applyMove (start, end) b = board'
  where
    sourcePice = posToPice start b
    board = setAt start (None 1) b
    board' = setAt end sourcePice board

type History = [Move]

applyMoves :: [Move] -> (Board, History) -> [(Board, History)]
applyMoves moves (board, history) = map f moves
  where
    f m = (applyMove m board, m : history)

rateMoves :: Int -> [[Move]] -> Color -> [(Board, History)] -> [(Int, History)]
rateMoves 0 _ c bhs = map f bhs where f (b, h) = (evaluateBoard c b, h)
rateMoves depth moves c boards
  | c == Red = map negate $ rateMoves (depth - 1) moves' Black boards'
  | c == Black = map negate $ rateMoves (depth - 1) moves' Red boards'
  where
    nextP
      | c == Red = Black
      | otherwise = Red
    negate (r, h) = (-r, h)
    boards' = concatMap (uncurry applyMoves) $ zip moves boards
    movesOf (b, _) = availableMoves nextP b
    moves' = map movesOf boards'

pickBest :: [(Int, History)] -> (Int, History)
pickBest = maximumBy (\(x, _) (y, _) -> compare x y)

-- end of Move evaluation

-- listing of Moves
diff :: Position -> Position -> Position
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

isInBound :: Position -> Bool
isInBound (x, y) = x >= 0 && x < 9 && y >= 0 && y < 10

filterPos :: [Position] -> [Position]
filterPos = filter isInBound

movesOfPice :: Pice -> Position -> [Move]
movesOfPice (None _) _ = []
movesOfPice (Rook _) (x, y) = (,) (x, y) <$> (hori ++ vert)
  where
    hori = (,) x <$> take 10 [0 ..]
    f xPos = (xPos, y)
    vert = map f $ take 9 [0 ..]
movesOfPice (Cano c) pos = movesOfPice (Rook c) pos
movesOfPice (Sold c) (x, y)
  | y < 5 && c == Red = (,) (x, y) <$> [(x, y + 1)]
  | y > 4 && c == Black = (,) (x, y) <$> [(x, y - 1)]
  | y > 4 && c == Red =
    (,) (x, y)
      <$> filterPos [(x, y + 1), (x + 1, y), (x - 1, y)]
  | y < 5 && c == Black =
    (,) (x, y)
      <$> filterPos [(x, y - 1), (x + 1, y), (x - 1, y)]
movesOfPice (Gene _) (x, y) =
  (,) (x, y) <$> filterPos [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
movesOfPice (Elep _) (x, y) =
  (,) (x, y)
    <$> filterPos
      [(x + 2, y + 2), (x - 2, y + 2), (x + 2, y - 2), (x - 2, y - 2)]
movesOfPice (Advi _) (x, y) =
  (,) (x, y)
    <$> filterPos
      [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]
movesOfPice (Hors _) (x, y) =
  (,) (x, y)
    <$> filterPos (upMoves ++ downMoves ++ leftMoves ++ rightMoves)
  where
    upMoves = [(x + 1, y + 2), (x - 1, y + 2)]
    downMoves = [(x + 1, y - 2), (x - 1, y - 2)]
    leftMoves = [(x - 2, y + 1), (x - 2, y - 1)]
    rightMoves = [(x + 2, y + 1), (x + 2, y - 1)]

posToPice :: Position -> Board -> Pice
posToPice key = f . Map.lookup key
  where
    f (Just pice) = pice
    f Nothing = None 1

isOpponent :: Pice -> Color -> Bool
isOpponent (None _) _ = False
isOpponent (Gene c1) c2 = c1 /= c2
isOpponent (Advi c1) c2 = c1 /= c2
isOpponent (Elep c1) c2 = c1 /= c2
isOpponent (Hors c1) c2 = c1 /= c2
isOpponent (Cano c1) c2 = c1 /= c2
isOpponent (Rook c1) c2 = c1 /= c2
isOpponent (Sold c1) c2 = c1 /= c2

verticalRow :: Int -> Board -> Row
verticalRow x b = map f $ take 10 [0 ..] where f y = posToPice (x, y) b

horizontalRow :: Int -> Board -> Row
horizontalRow y b = map f $ take 9 [0 ..] where f x = posToPice (x, y) b

minmax :: Int -> Int -> (Int, Int)
minmax x y
  | x < y = (x, y)
  | otherwise = (y, x)

-- assume the move is aligned in one row or colum
blockCount :: Move -> Board -> Int
blockCount (start, end) b
  | xDiff == 0 = length vertRow - count (== None 1) vertRow
  | yDiff == 0 = length horiRow - count (== None 1) horiRow
  where
    (xDiff, yDiff) = diff end start
    (xStart, yStart) = start
    (xEnd, yEnd) = end
    (lowerX, upperX) = minmax xStart xEnd
    (lowerY, upperY) = minmax yStart yEnd
    vertRow =
      drop (10 - upperY) . reverse . drop (lowerY + 1) $ verticalRow xStart b
    horiRow =
      drop (9 - upperX) . reverse . drop (lowerX + 1) $ horizontalRow yStart b
    count pre pices
      | null pices = 0
      | pre $ head pices = 1 + count pre (tail pices)
      | otherwise = count pre (tail pices)
blockCount _ _ = 0

-- assume the end position is in bound
isValidMove :: Pice -> Move -> Board -> Bool
isValidMove (None _) _ _ = False
isValidMove p (_, (x, y)) board
  | p == Gene Red || p == Advi Red =
    y >= 0 && y < 3 && x > 2 && x < 6 && (pice == None 1 || isOpponent pice Red)
  | p == Gene Black || p == Advi Black =
    y
      >= 7
      && y
      < 10
      && x
      > 2
      && x
      < 6
      && (pice == None 1 || isOpponent pice Black)
  where
    pice = posToPice (x, y) board
isValidMove (Hors c) (start, (x, y)) board =
  (pice == None 1 || isOpponent pice c)
    && ( (verticalMove && piceVert == None 1)
           || (horizontalMove && piceHori == None 1)
       )
  where
    pice = posToPice (x, y) board
    (xDiff, yDiff) = (x, y) `diff` start
    verticalMove = abs yDiff > abs xDiff
    horizontalMove = abs xDiff > abs yDiff
    (sX, sY) = start
    piceHori = posToPice (sX + quot xDiff 2, sY) board
    piceVert = posToPice (sX, sY + quot yDiff 2) board
isValidMove (Elep c) (start, (x, y)) board
  | c == Red =
    (pice == None 1 || isOpponent pice c) && piceBlock == None 1 && y < 5
  | c == Black =
    (pice == None 1 || isOpponent pice c) && piceBlock == None 1 && y > 4
  where
    pice = posToPice (x, y) board
    (xDiff, yDiff) = diff (x, y) start
    piceBlock = posToPice (diff start (quot (-xDiff) 2, quot (-yDiff) 2)) board
isValidMove (Sold c) (_, (x, y)) board = pice == None 1 || isOpponent pice c
  where
    pice = posToPice (x, y) board
isValidMove (Rook c) (start, (x, y)) board =
  (pice == None 1 || isOpponent pice c) && counts == 0
  where
    pice = posToPice (x, y) board
    counts = blockCount (start, (x, y)) board
isValidMove (Cano c) (start, (x, y)) board =
  (pice == None 1 && counts == 0) || (isOpponent pice c && counts == 1)
  where
    pice = posToPice (x, y) board
    counts = blockCount (start, (x, y)) board
isValidMove _ _ _ = False

-- assume the board is already expanded
playerPices :: Color -> Board -> Map.Map Position Pice
playerPices c b = Map.filter f b where f pice = not $ isOpponent pice c

-- end of listing of Moves
