{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.TestParser ( tests ) where

import Bot.Test.TestUtil
import Bot.Parser          ( parser, runParser )
import Control.Applicative ( (<$>), (<*>) )

readChoice cs a = if a `elem` cs then Just a else Nothing
readColor = readChoice ["red", "blue", "green"]
readShape = readChoice ["square", "triangle", "circle"]
colorParser = parser "color" "a color" readColor
shapeParser = parser "shape" "a shape" readShape
colorShapeParser = (,) <$> colorParser <*> shapeParser
parseColorShape = runParser colorShapeParser

case_ok = 
  parseColorShape ["blue", "square"] @?= Right ("blue", "square")

case_wrong_order = 
  parseColorShape ["square", "blue"] @?= 
    Left "Couldn't read color from 'square'. Expected: a color"

case_invalid_first_arg = 
  parseColorShape ["aaa", "square"] @?= 
    Left "Couldn't read color from 'aaa'. Expected: a color"

case_invalid_second_arg = 
  parseColorShape ["blue", "aaa"] @?= 
    Left "Couldn't read shape from 'aaa'. Expected: a shape"

case_too_many_args = 
  parseColorShape ["blue", "square", "aaa", "bbb"] @?= 
    Left "Unused arguments: aaa, bbb"

tests = $(testGroupGenerator)