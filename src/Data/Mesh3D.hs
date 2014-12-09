{-
Copyright (C) 2014 Braden Walters

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.
-}

module Data.Mesh3D
( Mesh3D(..)
, Vertex(..)
, Frame(..)
) where

data Vertex =
  Vertex
  { position :: (Float, Float, Float)
  , normal :: (Float, Float, Float) }
  deriving (Show)

data Frame =
  Frame
  { name :: String
  , vertices :: [Vertex] }
  deriving (Show)

data Mesh3D =
  Mesh3D
  { textureSize :: (Int, Int)
  , frames :: [Frame] }
  deriving (Show)
