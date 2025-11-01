{-# OPTIONS_GHC -Wno-type-defaults #-}

module Geometry (Point2D, distance2, Line2D, length2) where

import GHC.Float (sqrtDouble)

data Point2D = Point2D Int Int deriving (Eq)

instance Show Point2D where
  show (Point2D x y) = "Point2D(" ++ (show x) ++ ", " ++ (show y) ++ ")"

distance2 :: Point2D -> Point2D -> Double
distance2 (Point2D x1 y1) (Point2D x2 y2) = sqrtDouble $ fromIntegral (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))

data Line2D = Line2D Point2D Point2D deriving (Eq)

instance Show Line2D where
  show (Line2D p1 p2) = foldr (++) "" ["Line2D(", show p1, ", ", show p2, ")"]

length2 :: Line2D -> Double
length2 (Line2D p1 p2) = distance2 p1 p2
