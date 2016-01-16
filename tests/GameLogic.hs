{-# LANGUAGE TemplateHaskell #-}
module GameLogic (testGroup)
where

import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Tetris2D

import Data.Map (fromList)

-- Orientation rotates into itself every fourth time
prop_rotate_orientation :: Orientation -> Positive Int -> Bool
prop_rotate_orientation o (Positive n) = (o == (iterate rotateRight o) !! n) == (mod n 4 == 0)

-- Initial position is valid
prop_initial_fine :: Figure -> Orientation -> Figure -> Orientation -> Bool
prop_initial_fine randomFigure1 randomOrient1 randomFigure2 randomOrient2 =
    isValid $ emptyField randomFigure1 randomOrient1 randomFigure2 randomOrient2

-- All figures are composed of four boxes, hence "tetris".
prop_tetris_four :: Figure -> Orientation -> Bool
prop_tetris_four = (((==4) . length . (concatMap (filter id)) ) . ) . rotatedShape

prop_drop_down :: Figure -> Orientation -> Figure -> Orientation -> Bool
prop_drop_down randomFigure1 randomOrient1 randomFigure2 randomOrient2 =
  dropDown game == (iterate moveDown game) !! fieldHeight
    where
    game = emptyField randomFigure1 randomOrient1 randomFigure2 randomOrient2


-- One figure on the bottom is valid
case_box_on_bottom :: Assertion
case_box_on_bottom = assert (isValid field)
  where field = (emptyField FigO N FigL N) {
    _currentFigure = FigO,
    _currentPosition = Position (Coord 2 (fieldHeight - 2)) N,
    _gameState = InProgress
  }

-- One figure on the bottom can't move down
case_box_on_bottom_cant_move_down :: Assertion
case_box_on_bottom_cant_move_down = field @=? moveDown field
  where field = (emptyField FigO N FigL N) {
    _currentFigure = FigO,
    _currentPosition = Position (Coord 2 (fieldHeight - 2)) N,
    _gameState = InProgress
  }

-- A figure has just fell, becomes part of the game and rows get deleted
case_box_just_fell :: Assertion
case_box_just_fell = (expected, 10 + 3, 123 + 300) @=? (_debris tested, _lines tested, _scores tested)
  where
  tested = fromLeft $ nextStep field
  field = (emptyField FigO N FigO N) {
    _debris = createDebris ((Coord 4 6):(Coord 3 3):(Coord 1 1):(Coord 1 0)
        :[Coord x y | x <- [0..fieldWidth - 1], y <- [2,4,5]]),
    _currentFigure = FigO,
    _currentPosition = Position (Coord 0 (fieldHeight - 1 - 7)) N,
    _gameState = InProgress,
    _scores = 123,
    _lines = 10
  }
  createDebris ds = fromList $ zip (map (\(Coord a b) -> Coord a (fieldHeight - 1 - b)) ds)
                                   (repeat FigO)
  fromLeft (Left a) = a (FigO, N)
  fromLeft (Right a) = error $ "unexpected " ++ show a
  expected = createDebris [Coord 4 3, Coord 3 2, Coord 1 1, Coord 1 0,
                           Coord 0 3, Coord 1 3, Coord 0 4, Coord 1 4]


instance Arbitrary Figure where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Orientation where
  arbitrary = arbitraryBoundedEnum

testGroup = $(testGroupGenerator)

