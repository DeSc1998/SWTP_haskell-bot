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
import Data.Maybe
import GHC.Utils.Misc
import Util

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

type Board = [Row]

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

piceToChar :: Pice -> Char
piceToChar (None n) = chr n
piceToChar (Gene c)
  | c == Red = 'G'
  | c == Black = 'g'
piceToChar (Advi c)
  | c == Red = 'A'
  | c == Black = 'a'
piceToChar (Elep c)
  | c == Red = 'E'
  | c == Black = 'e'
piceToChar (Hors c)
  | c == Red = 'H'
  | c == Black = 'h'
piceToChar (Rook c)
  | c == Red = 'R'
  | c == Black = 'r'
piceToChar (Cano c)
  | c == Red = 'C'
  | c == Black = 'c'
piceToChar (Sold c)
  | c == Red = 'S'
  | c == Black = 's'

convertPos :: Position -> String
convertPos (x, y) = [chr (x + ord 'a'), chr (y + ord '0')]

convertMove :: Move -> String
convertMove (start, end) = convertPos start ++ "-" ++ convertPos end

charPices :: String
charPices = "GAEHRCSgaehrcs123456789"

charsHori :: String
charsHori = "abcdefghi"

expandNone :: Pice -> [Pice]
expandNone (None n)
  | n > 1 = None 1 : expandNone (None (n - 1))
  | n == 1 = [None 1]
  | n < 1 = []
expandNone p = [p]

expand :: [Pice] -> [Pice]
expand = concatMap expandNone

collapse :: [Pice] -> [Pice]
collapse ((None n) : None 1 : rest) = collapse (None (n + 1) : rest)
collapse (p : rest) = p : collapse rest
collapse [] = []

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs)
      | x == c = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

predicateP :: (Char -> Bool) -> Parser Char
predicateP pre = Parser f
  where
    f (x : xs)
      | pre x = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

pPice :: Char -> Parser Pice
pPice c = charToPice <$> charP c

piceP :: Parser Pice
piceP = foldMap pPice charPices

rowP :: Parser Row
rowP = many piceP

boardP :: Parser Board
boardP = many (rowP <* (charP '/' <|> charP ' '))

fieldP :: Parser Position
fieldP = (\x y -> (,) (ord x - ord 'a') (ord y - ord '0')) <$> horiP <*> vertP
  where
    vertP = predicateP isDigit
    horiP = foldMap charP charsHori

moveP :: Parser Move
moveP = (,) <$> (fieldP <* charP '-') <*> fieldP

moveListP :: Parser [Move]
moveListP = many (moveP <* charP ';')

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
    f b p = (reverse b, p)

-- TODO: 'getMove' currently only picks the first move
--       needs proper implementation
getMove :: String -> String
getMove = convertMove . head . fst . fromJust . run moveListP

diff :: Position -> Position -> Position
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

isInBound :: Position -> Bool
isInBound (x, y) = x >= 0 && x < 9 && y >= 0 && y < 10

filterPos :: [Position] -> [Position]
filterPos = filter isInBound

movesOfPice :: Pice -> Position -> [Move]
movesOfPice (None _) pos = []
movesOfPice (Rook c) (x, y) = (,) (x, y) <$> filterPos (hori ++ vert)
  where
    hori = (,) x <$> take 10 [0 ..]
    f x = (x, y)
    vert = map f $ take 10 [0 ..]
movesOfPice (Cano c) pos = movesOfPice (Rook c) pos
movesOfPice (Sold c) (x, y)
  | y < 5 && c == Red = (,) (x, y) <$> [(x, y + 1)]
  | y > 4 && c == Black = (,) (x, y) <$> [(x, y - 1)]
  | y > 4 && c == Red = (,) (x, y) <$> filterPos [(x, y + 1), (x + 1, y), (x - 1, y)]
  | y < 5 && c == Black = (,) (x, y) <$> filterPos [(x, y - 1), (x + 1, y), (x - 1, y)]
movesOfPice (Gene c) (x, y) = (,) (x, y) <$> filterPos [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
movesOfPice (Elep c) (x, y) = (,) (x, y) <$> filterPos [(x + 2, y + 2), (x - 2, y + 2), (x + 2, y - 2), (x - 2, y - 2)]
movesOfPice (Advi c) (x, y) = (,) (x, y) <$> filterPos [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]
movesOfPice (Hors c) (x, y) = (,) (x, y) <$> filterPos (upMoves ++ downMoves ++ leftMoves ++ rightMoves)
  where
    upMoves = [(x + 1, y + 2), (x - 1, y + 2)]
    downMoves = [(x + 1, y - 2), (x - 1, y - 2)]
    leftMoves = [(x - 2, y + 1), (x - 2, y - 1)]
    rightMoves = [(x + 2, y + 1), (x + 2, y - 1)]

at :: Int -> [a] -> a
at 0 (x : _) = x
at n (x : xs)
  | n > 0 = at (n - 1) xs
  | otherwise = undefined

posToPice :: Position -> Board -> Pice
posToPice (x, y) b
  | isInBound (x, y) = at x $ at y b
  | otherwise = None 0

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
verticalRow x b = map f $ take 10 [0 ..]
  where
    f y = posToPice (x, y) b

horizontalRow :: Int -> Board -> Row
horizontalRow = at

-- assume the move is only in one row or colum
blockCount :: Move -> Board -> Int
blockCount (start, end) b
  | xDiff == 0 = length vertRow - count (== None 1) vertRow
  | yDiff == 0 = length horiRow - count (== None 1) horiRow
  where
    (xDiff, yDiff) = diff end start
    (x, y) = start
    vertRow = tail . reverse . tail $ verticalRow x b
    horiRow = tail . reverse . tail $ horizontalRow y b

-- assume the end position is in bound
isValidMove :: Pice -> Move -> Board -> Bool
isValidMove (None _) (start, end) board = False
isValidMove p (start, (x, y)) board
  | p == Gene Red || p == Advi Red = y >= 0 && y < 3 && x > 2 && x < 6 && (pice == None 1 || isOpponent pice Red)
  | p == Gene Black || p == Advi Black = y >= 7 && y < 10 && x > 2 && x < 6 && (pice == None 1 || isOpponent pice Black)
  where
    pice = posToPice (x, y) board
isValidMove (Hors c) (start, (x, y)) board = (pice == None 1 || isOpponent pice c) && ((verticalMove && piceVert == None 1) || (horizontalMove && piceHori == None 1))
  where
    pice = posToPice (x, y) board
    (xDiff, yDiff) = diff (x, y) start
    verticalMove = abs xDiff > abs yDiff
    horizontalMove = abs yDiff > abs xDiff
    piceHori = posToPice (x + quot xDiff 2, y) board
    piceVert = posToPice (x, y + quot yDiff 2) board
isValidMove (Elep c) (start, (x, y)) board
  | c == Red = (pice == None 1 || isOpponent pice c) && piceBlock == None 1 && y < 5
  | c == Black = (pice == None 1 || isOpponent pice c) && piceBlock == None 1 && y > 4
  where
    pice = posToPice (x, y) board
    (xDiff, yDiff) = diff (x, y) start
    piceBlock = posToPice (diff start (quot (-xDiff) 2, quot (-yDiff) 2)) board
isValidMove (Sold c) (_, (x, y)) board = pice == None 1 || isOpponent pice c
  where
    pice = posToPice (x, y) board
isValidMove (Rook c) (start, (x, y)) board = (pice == None 1 || isOpponent pice c) && counts == 0
  where
    pice = posToPice (x, y) board
    counts = blockCount (start, (x, y)) board
isValidMove (Cano c) (start, (x, y)) board = (pice == None 1 && counts == 0) || (isOpponent pice c && counts == 1)
  where
    pice = posToPice (x, y) board
    counts = blockCount (start, (x, y)) board

indexPices :: Position -> Row -> [(Pice, Position)]
indexPices (x, y) (pice : pices) = (pice, (x, y)) : indexPices (x + 1, y) pices
indexPices _ [] = []

-- assume the board is already expanded
playerPices :: Color -> Board -> [(Pice, Position)]
playerPices c b = filter f $ concat indexedBoard
  where
    f (pice, pos) = not $ isOpponent pice c
    g (y, row) = indexPices (0, y) row
    indexedBoard = zipWith (curry g) (take 10 [0 ..]) b

listMoves :: String -> String
listMoves strBoard = concatMap convert moves
  where
    (board, player) = fst . fromJust $ run configP strBoard
    listPiceMoves (pice, pos) = (pice, movesOfPice pice pos)
    valid pice move = isValidMove pice move board
    filterValidMoves (pice, moves) = filter (valid pice) moves
    moves = concatMap (filterValidMoves . listPiceMoves) $ playerPices player $ map expand board
    convert m = convertMove m ++ ";"
