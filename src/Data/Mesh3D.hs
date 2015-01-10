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
, Normal(..)
, TextureCoordinates(..)
, Triangle(..)
, Frame(..)
, ImageData(..)
, TextureContents(..)
, Texture(..)
) where

import qualified Data.ByteString.Lazy as LBS

data Vertex = Vertex (Float, Float, Float) deriving (Show, Eq)
data Normal = Normal (Float, Float, Float) deriving (Show, Eq)
data TextureCoordinates = TextureCoordinates (Float, Float) deriving (Show, Eq)

data Triangle =
  Triangle
  { triVertexIndices :: (Int, Int, Int)
  , triNormalIndices :: Maybe (Int, Int, Int)
  , triTextureCoordinateIndices :: Maybe (Int, Int, Int) }
  deriving (Show)

data Frame =
  Frame
  { frameName :: Maybe String
  , frameVertices :: [Vertex]
  , frameNormals :: [Normal] }
  deriving (Show)

newtype ImageData = ImageData LBS.ByteString deriving (Show)
data TextureContents = TexturePath FilePath | TextureData ImageData
                       deriving (Show)
data Texture =
  Texture
  { texContents :: TextureContents
  , texSize :: (Int, Int) }
  deriving (Show)

data Mesh3D =
  Mesh3D
  { frames :: [Frame]
  , texCoords :: [TextureCoordinates]
  , triangles :: [Triangle]
  , textures :: [Texture] }
  deriving (Show)
