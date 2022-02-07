module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

-- DIESES MODUL NICHT ÄNDERN!!!

import System.Environment
import qualified Data.List as List

import XiangqiBot

-- Wer hier mehr erfahren will: Im naechsten Schritt (nicht Teil des Stoffs) kann in Haskell
-- mit Monaden impliziter ein Zustand definiert und in sequentieller Ausfuehrung mitgefuehrt werden
-- Dadurch wird auch Interaktion mit der Umgebung moeglich, wie im Beispiel der IO-Monade.
main :: IO ()
main = do
  args <- getArgs
  let oneString = concat (List.intersperse " " args)
  putStrLn ( getMove oneString )