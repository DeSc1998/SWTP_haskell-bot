{-# LANGUAGE DeriveFunctor #-}

-- module (NICHT ÄNDERN!)
module XiangqiBot
  ( getMove
  , listMoves
  ) where

-- More modules may be imported
import           Data.Char
import           Util

import           Control.Applicative

data Color = Red
           | Black
           deriving (Show, Eq)

data Pice = None Int
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

newtype Parser a = Parser { run :: String -> Maybe (a, String) }
  deriving Functor


instance Applicative Parser where
  pure x = Parser $ \y -> Just (x, y)
  p1 <*> p2 = Parser
    (\input -> do
      (f, input' ) <- run p1 input
      (a, input'') <- run p2 input'
      Just (f a, input'')
    )

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

piceColor :: Char -> Color
piceColor c | isUpper c = Red
            | isLower c = Black
            | otherwise = undefined

charToPice :: Char -> Pice
charToPice c | toUpper c == 'G' = Gene (piceColor c)
             | toUpper c == 'A' = Advi (piceColor c)
             | toUpper c == 'E' = Elep (piceColor c)
             | toUpper c == 'H' = Hors (piceColor c)
             | toUpper c == 'R' = Rook (piceColor c)
             | toUpper c == 'C' = Cano (piceColor c)
             | toUpper c == 'S' = Sold (piceColor c)
             | isDigit c        = None (ord c - ord '0')
             | otherwise        = undefined

piceToChar :: Pice -> Char
piceToChar (None n) = chr n
piceToChar (Gene c) | c == Red   = 'G'
                    | c == Black = 'g'
piceToChar (Advi c) | c == Red   = 'A'
                    | c == Black = 'a'
piceToChar (Elep c) | c == Red   = 'E'
                    | c == Black = 'e'
piceToChar (Hors c) | c == Red   = 'H'
                    | c == Black = 'h'
piceToChar (Rook c) | c == Red   = 'R'
                    | c == Black = 'r'
piceToChar (Cano c) | c == Red   = 'C'
                    | c == Black = 'c'
piceToChar (Sold c) | c == Red   = 'S'
                    | c == Black = 's'

charPices :: String
charPices = "GAEHRCSgaehrcs123456789"

charsVert :: String
charsVert = "0123456789"

charsHori :: String
charsHori = "abcdefghi"

expandNone :: Pice -> [Pice]
expandNone (None n) | n > 1  = None 1 : expandNone (None (n - 1))
                    | n == 1 = [None 1]
                    | n < 1  = []
expandNone p = [p]

collapseNone :: [Pice] -> [Pice]
collapseNone = undefined

charP :: Char -> Parser Char
charP c = Parser f where
  f input | head input == c = Just (c, tail input)
          | otherwise       = Nothing

pPice :: Char -> Parser Pice
pPice c = charToPice <$> charP c

piceP :: Parser Pice
piceP = foldl (<|>) empty $ map pPice charPices

rowP :: Parser Row
rowP = many piceP

boardP :: Parser Board
boardP = many ((rowP <* charP '/') <|> rowP)

vertP = foldl (<|>) empty $ map charP charsVert
horiP = foldl (<|>) empty $ map charP charsHori

fieldP :: Parser Position
fieldP = (\x y -> (,) (digitToInt x) (ord y - ord 'a')) <$> vertP <*> horiP

moveP :: Parser Move
moveP = (,) <$> (fieldP <* charP '-') <*> fieldP

moveListP :: Parser [Move]
moveListP = many $ (moveP <* charP ';') <|> moveP



--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove = undefined
-- input = do
--   (moves, _) <- run moveListP input
--   head moves


listMoves :: String -> String
listMoves xs = xs -- YOUR IMPLEMENTATION HERE


-- YOUR IMPLEMENTATION FOLLOWS HERE

