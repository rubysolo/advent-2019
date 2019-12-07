module Main where

import Prelude
import Data.Array (head, index, updateAt)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let input = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,9,23,27,2,27,6,31,1,5,31,35,2,9,35,39,2,6,39,43,2,43,13,47,2,13,47,51,1,10,51,55,1,9,55,59,1,6,59,63,2,63,9,67,1,67,6,71,1,71,13,75,1,6,75,79,1,9,79,83,2,9,83,87,1,87,6,91,1,91,13,95,2,6,95,99,1,10,99,103,2,103,9,107,1,6,107,111,1,10,111,115,2,6,115,119,1,5,119,123,1,123,13,127,1,127,5,131,1,6,131,135,2,135,13,139,1,139,2,143,1,143,10,0,99,2,0,14,0"
  log $ show $ run input

run :: String -> Int
run initial =
  parse initial
    # execute 0
    # head
    # fromMaybe (-1)

parse :: String -> Array Int
parse input =
  split (Pattern ",") input
    # map fromString
    # map (fromMaybe (-1))

execute :: Int -> Array Int -> Array Int
execute pos state =
  let
    op = valueAt state pos
  in
    next op pos state

next :: Int -> Int -> Array Int -> Array Int
next 1 pos state = continue (\l r -> l + r) pos state
next 2 pos state = continue (\l r -> l * r) pos state
next 99 pos state = state
next op pos state = []

continue :: (Int -> Int -> Int) -> Int -> Array Int -> Array Int
continue fn pos state =
  let
    left = valueAt state $ valueAt state (pos + 1)
    right = valueAt state $ valueAt state (pos + 2)
    target = valueAt state (pos + 3)
    nextState = updatedState target (fn left right) state
  in
    execute (pos + 4) nextState

updatedState :: Int -> Int -> Array Int -> Array Int
updatedState address value state =
  updateAt address value state
    # fromMaybe []

valueAt :: Array Int -> Int -> Int
valueAt vals pos =
  index vals pos
    # fromMaybe (-1)