module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, uncons)
import Data.String.CodePoints (CodePoint, codePointFromChar, singleton)

main :: Effect Unit
main = do
  log "Hello sailor!"

data Point x y = Point x y

instance showPoint :: (Show a, Show b) => Show (Point a b) where
  show (Point a b) = "(" <> show a <> ", " <> show b <> ")"

derive instance eqPoint :: (Eq a, Eq b) => Eq (Point a b)

data Direction = Up | Down | Left | Right

data Move = Move Direction Int
derive instance eqMove :: (Eq a, Eq b) => Eq (Move a b)


--allPoints :: String -> Array (Point Int Int)
allPoints :: String -> Array Move
allPoints path =
  moves path


moves :: String -> Array Move
moves input =
    split (Pattern ",") input
      # map uncons
      # map move

move :: Maybe { head :: CodePoint, tail :: String} -> Move
move Nothing = Move Up 0
move (Just { head, tail }) = Move (direction $ singleton head) (distance tail)

direction :: String -> Direction
direction "U" = Up
direction "D" = Down
direction "L" = Left
direction _ = Right

distance :: String -> Int
distance s =
  fromString s
    # fromMaybe 0