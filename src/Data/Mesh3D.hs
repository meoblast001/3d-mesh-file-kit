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
, TextureCoordinates(..)
, Triangle(..)
, Frame(..)
) where

data Vertex =
  Vertex
  { vertPosition :: (Float, Float, Float)
  , vertNormal :: (Float, Float, Float) }
  deriving (Show)

data TextureCoordinates =
  TextureCoordinates
  { texCoordX :: Float
  , texCoordY :: Float }
  deriving (Show)

data Triangle =
  Triangle
  { triVertexIndices :: (Int, Int, Int)
  , triTextureCoordinates :: (TextureCoordinates, TextureCoordinates,
                           TextureCoordinates)
  , triTextureCoordinateIndices :: (Int, Int, Int) }
  deriving (Show)

data Frame =
  Frame
  { frameName :: String
  , frameVertices :: [Vertex] }
  deriving (Show)

data Mesh3D =
  Mesh3D
  { textureSize :: (Int, Int)
  , frames :: [Frame]
  , texCoords :: [TextureCoordinates]
  , triangles :: [Triangle]}
  deriving (Show)
