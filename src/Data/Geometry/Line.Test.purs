module Data.Geometry.Line.Test where

import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Line

import Math

import Test.Mocha
import Test.Chai
import Test.QuickCheck

mkLine :: Number -> Number -> Number -> Number -> Line Number
mkLine x y x' y' = Line (Point x y) (Point x' y')

foreign import toFixed """
  function toFixed(decimal){
    return function(n){
      return n.toFixed(decimal);
    };
  }
""" :: Number -> Number -> Number

fuzz = toFixed 2

init = describe "Line" do 
  
  it "perimeter" $ quickCheck \y y' ->
    let 
      l   = Line (Point 0 y) (Point 0 y')
      l'  = Line (Point y 0) (Point y' 0)
      f x = fuzz (perimeter x) == fuzz (abs (y - y'))
    in f l && f l'

  it "eq" $ quickCheck \x y x' y' ->
    let l = mkLine x y x' y'
    in  l == l && l /= Line (Point x y) (Point x' (y' + 1))

  it "area" $ quickCheck \x y x' y' ->
    let l = mkLine x y x' y' in area l == 0

  it "points" $ quickCheck \x y x' y' ->
    let l = mkLine x y x' y' in points l == [Point x y, Point x' y']