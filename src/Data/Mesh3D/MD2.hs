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

module Data.Mesh3D.MD2 ( load ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as LBS
import Data.Mesh3D
import Language.Haskell.TH.Ppr

normalVectors :: [(Float, Float, Float)]
normalVectors = [
    (-0.525731, 0.000000, 0.850651),
    (-0.442863, 0.238856, 0.864188),
    (-0.295242, 0.000000, 0.955423),
    (-0.309017, 0.500000, 0.809017),
    (-0.162460, 0.262866, 0.951056),
    (0.000000, 0.000000, 1.000000),
    (0.000000, 0.850651, 0.525731),
    (-0.147621, 0.716567, 0.681718),
    (0.147621, 0.716567, 0.681718),
    (0.000000, 0.525731, 0.850651),
    (0.309017, 0.500000, 0.809017),
    (0.525731, 0.000000, 0.850651),
    (0.295242, 0.000000, 0.955423),
    (0.442863, 0.238856, 0.864188),
    (0.162460, 0.262866, 0.951056),
    (-0.681718, 0.147621, 0.716567),
    (-0.809017, 0.309017, 0.500000),
    (-0.587785, 0.425325, 0.688191),
    (-0.850651, 0.525731, 0.000000),
    (-0.864188, 0.442863, 0.238856),
    (-0.716567, 0.681718, 0.147621),
    (-0.688191, 0.587785, 0.425325),
    (-0.500000, 0.809017, 0.309017),
    (-0.238856, 0.864188, 0.442863),
    (-0.425325, 0.688191, 0.587785),
    (-0.716567, 0.681718, -0.147621),
    (-0.500000, 0.809017, -0.309017),
    (-0.525731, 0.850651, 0.000000),
    (0.000000, 0.850651, -0.525731),
    (-0.238856, 0.864188, -0.442863),
    (0.000000, 0.955423, -0.295242),
    (-0.262866, 0.951056, -0.162460),
    (0.000000, 1.000000, 0.000000),
    (0.000000, 0.955423, 0.295242),
    (-0.262866, 0.951056, 0.162460),
    (0.238856, 0.864188, 0.442863),
    (0.262866, 0.951056, 0.162460),
    (0.500000, 0.809017, 0.309017),
    (0.238856, 0.864188, -0.442863),
    (0.262866, 0.951056, -0.162460),
    (0.500000, 0.809017, -0.309017),
    (0.850651, 0.525731, 0.000000),
    (0.716567, 0.681718, 0.147621),
    (0.716567, 0.681718, -0.147621),
    (0.525731, 0.850651, 0.000000),
    (0.425325, 0.688191, 0.587785),
    (0.864188, 0.442863, 0.238856),
    (0.688191, 0.587785, 0.425325),
    (0.809017, 0.309017, 0.500000),
    (0.681718, 0.147621, 0.716567),
    (0.587785, 0.425325, 0.688191),
    (0.955423, 0.295242, 0.000000),
    (1.000000, 0.000000, 0.000000),
    (0.951056, 0.162460, 0.262866),
    (0.850651, -0.525731, 0.000000),
    (0.955423, -0.295242, 0.000000),
    (0.864188, -0.442863, 0.238856),
    (0.951056, -0.162460, 0.262866),
    (0.809017, -0.309017, 0.500000),
    (0.681718, -0.147621, 0.716567),
    (0.850651, 0.000000, 0.525731),
    (0.864188, 0.442863, -0.238856),
    (0.809017, 0.309017, -0.500000),
    (0.951056, 0.162460, -0.262866),
    (0.525731, 0.000000, -0.850651),
    (0.681718, 0.147621, -0.716567),
    (0.681718, -0.147621, -0.716567),
    (0.850651, 0.000000, -0.525731),
    (0.809017, -0.309017, -0.500000),
    (0.864188, -0.442863, -0.238856),
    (0.951056, -0.162460, -0.262866),
    (0.147621, 0.716567, -0.681718),
    (0.309017, 0.500000, -0.809017),
    (0.425325, 0.688191, -0.587785),
    (0.442863, 0.238856, -0.864188),
    (0.587785, 0.425325, -0.688191),
    (0.688191, 0.587785, -0.425325),
    (-0.147621, 0.716567, -0.681718),
    (-0.309017, 0.500000, -0.809017),
    (0.000000, 0.525731, -0.850651),
    (-0.525731, 0.000000, -0.850651),
    (-0.442863, 0.238856, -0.864188),
    (-0.295242, 0.000000, -0.955423),
    (-0.162460, 0.262866, -0.951056),
    (0.000000, 0.000000, -1.000000),
    (0.295242, 0.000000, -0.955423),
    (0.162460, 0.262866, -0.951056),
    (-0.442863, -0.238856, -0.864188),
    (-0.309017, -0.500000, -0.809017),
    (-0.162460, -0.262866, -0.951056),
    (0.000000, -0.850651, -0.525731),
    (-0.147621, -0.716567, -0.681718),
    (0.147621, -0.716567, -0.681718),
    (0.000000, -0.525731, -0.850651),
    (0.309017, -0.500000, -0.809017),
    (0.442863, -0.238856, -0.864188),
    (0.162460, -0.262866, -0.951056),
    (0.238856, -0.864188, -0.442863),
    (0.500000, -0.809017, -0.309017),
    (0.425325, -0.688191, -0.587785),
    (0.716567, -0.681718, -0.147621),
    (0.688191, -0.587785, -0.425325),
    (0.587785, -0.425325, -0.688191),
    (0.000000, -0.955423, -0.295242),
    (0.000000, -1.000000, 0.000000),
    (0.262866, -0.951056, -0.162460),
    (0.000000, -0.850651, 0.525731),
    (0.000000, -0.955423, 0.295242),
    (0.238856, -0.864188, 0.442863),
    (0.262866, -0.951056, 0.162460),
    (0.500000, -0.809017, 0.309017),
    (0.716567, -0.681718, 0.147621),
    (0.525731, -0.850651, 0.000000),
    (-0.238856, -0.864188, -0.442863),
    (-0.500000, -0.809017, -0.309017),
    (-0.262866, -0.951056, -0.162460),
    (-0.850651, -0.525731, 0.000000),
    (-0.716567, -0.681718, -0.147621),
    (-0.716567, -0.681718, 0.147621),
    (-0.525731, -0.850651, 0.000000),
    (-0.500000, -0.809017, 0.309017),
    (-0.238856, -0.864188, 0.442863),
    (-0.262866, -0.951056, 0.162460),
    (-0.864188, -0.442863, 0.238856),
    (-0.809017, -0.309017, 0.500000),
    (-0.688191, -0.587785, 0.425325),
    (-0.681718, -0.147621, 0.716567),
    (-0.442863, -0.238856, 0.864188),
    (-0.587785, -0.425325, 0.688191),
    (-0.309017, -0.500000, 0.809017),
    (-0.147621, -0.716567, 0.681718),
    (-0.425325, -0.688191, 0.587785),
    (-0.162460, -0.262866, 0.951056),
    (0.442863, -0.238856, 0.864188),
    (0.162460, -0.262866, 0.951056),
    (0.309017, -0.500000, 0.809017),
    (0.147621, -0.716567, 0.681718),
    (0.000000, -0.525731, 0.850651),
    (0.425325, -0.688191, 0.587785),
    (0.587785, -0.425325, 0.688191),
    (0.688191, -0.587785, 0.425325),
    (-0.955423, 0.295242, 0.000000),
    (-0.951056, 0.162460, 0.262866),
    (-1.000000, 0.000000, 0.000000),
    (-0.850651, 0.000000, 0.525731),
    (-0.955423, -0.295242, 0.000000),
    (-0.951056, -0.162460, 0.262866),
    (-0.864188, 0.442863, -0.238856),
    (-0.951056, 0.162460, -0.262866),
    (-0.809017, 0.309017, -0.500000),
    (-0.864188, -0.442863, -0.238856),
    (-0.951056, -0.162460, -0.262866),
    (-0.809017, -0.309017, -0.500000),
    (-0.681718, 0.147621, -0.716567),
    (-0.681718, -0.147621, -0.716567),
    (-0.850651, 0.000000, -0.525731),
    (-0.688191, 0.587785, -0.425325),
    (-0.587785, 0.425325, -0.688191),
    (-0.425325, 0.688191, -0.587785),
    (-0.425325, -0.688191, -0.587785),
    (-0.587785, -0.425325, -0.688191),
    (-0.688191, -0.587785, -0.425325)
  ]

data LoadError = InvalidIdentifier | NormalNotFound | TextureCoordinatesNotFound
                 deriving (Show)

data VertexNormalPair = VertexNormalPair Vertex Normal

load :: LBS.ByteString -> Either LoadError Mesh3D
load data_in =
  let mainFunc = do
        ident <- getWord32le
        version <- getWord32le

        skin_width <- fromIntegral <$> getWord32le
        skin_height <- fromIntegral <$> getWord32le

        frame_size <- getWord32le

        num_skins <- fromIntegral <$> getWord32le
        num_verts <- fromIntegral <$> getWord32le
        num_tex_coords <- fromIntegral <$> getWord32le
        num_tris <- fromIntegral <$> getWord32le
        num_glcmds <- fromIntegral <$> getWord32le
        num_frames <- fromIntegral <$> getWord32le

        offset_skins <- fromIntegral <$> getWord32le
        offset_tex_coords <- fromIntegral <$> getWord32le
        offset_tris <- fromIntegral <$> getWord32le
        offset_frames <- fromIntegral <$> getWord32le
        offset_gl_commands <- fromIntegral <$> getWord32le
        offset_end <- fromIntegral <$> getWord32le

        let frames_e = sequence $ runGet (loadFrames num_frames num_verts)
                                         (LBS.drop offset_frames data_in)
            tex_coords = runGet (loadTexCoords num_tex_coords skin_width
                                               skin_height)
                                (LBS.drop offset_tex_coords data_in)
            triangles_e = sequence $ runGet (loadTriangles num_tris tex_coords)
                                            (LBS.drop offset_tris data_in)
            textures = runGet (loadTextures num_skins (skin_width, skin_height))
                              (LBS.drop offset_skins data_in)

        if ident == 844121161 then -- "IDP2" as integer.
          return $
            Mesh3D <$>
              frames_e <*>
              return tex_coords <*>
              triangles_e <*>
              return textures
        else
          return $ Left InvalidIdentifier
  in runGet mainFunc data_in

loadFrames :: Int -> Int -> Get [Either LoadError Frame]
loadFrames 0 _ = return []
loadFrames remaining_frames num_vertices = do
  scale <- liftM3 (,,) getFloat32le getFloat32le getFloat32le
  translate <- liftM3 (,,) getFloat32le getFloat32le getFloat32le
  name <- bytesToString . takeWhile (/= 0) <$> mapM (const getWord8) [1..8]
  vertex_normal_pairs_e <- sequence <$> loadVerticesNormals num_vertices scale
                                                            translate

  remaining <- getRemainingLazyByteString
  let rest = runGet (loadFrames (remaining_frames - 1) num_vertices) remaining

  case vertex_normal_pairs_e of
    Right vertex_normal_pairs ->
      let verts = map (\(VertexNormalPair v _) -> v) vertex_normal_pairs
          norms = map (\(VertexNormalPair _ n) -> n) vertex_normal_pairs
      in return $ Right Frame {
          frameName = Just name,
          frameVertices = verts,
          frameNormals = norms
        }:rest
    Left error -> return $ Left error:rest

loadVerticesNormals :: Int -> (Float, Float, Float) -> (Float, Float, Float) ->
                       Get [Either LoadError VertexNormalPair]
loadVerticesNormals 0 _ _ = return []
loadVerticesNormals remaining_verts scale@(sx, sy, sz)
                    translate@(tx, ty, tz) = do
  let transform s_coord t_coord coord = coord * s_coord + t_coord
  position <- liftM3 (,,) (transform sx tx <$> fromIntegral <$> getWord8)
                          (transform sy ty <$> fromIntegral <$> getWord8)
                          (transform sz tz <$> fromIntegral <$> getWord8)
  normal_m <- fmap (\i -> if i < length normalVectors
                          then Just (normalVectors !! i)
                          else Nothing) (fromIntegral <$> getWord8)

  remaining <- getRemainingLazyByteString
  let rest = runGet (loadVerticesNormals (remaining_verts - 1) scale translate)
                    remaining

  case normal_m of
    Just normal ->
      return $ (Right $ VertexNormalPair (Vertex position) (Normal normal)):rest
    Nothing -> return $ Left NormalNotFound:rest

loadTexCoords :: Int -> Int -> Int -> Get [TextureCoordinates]
loadTexCoords 0 _ _ = return []
loadTexCoords remaining_tex_coords skin_width skin_height = do
  s <- fmap (\x -> x / fromIntegral skin_width) $ fromIntegral <$> getWord16le
  t <- fmap (\x -> x / fromIntegral skin_height) $ fromIntegral <$> getWord16le

  remaining <- getRemainingLazyByteString
  return $ (TextureCoordinates (s, t)):
           runGet (loadTexCoords (remaining_tex_coords - 1)
                                 skin_width skin_height) remaining

loadTriangles :: Int -> [TextureCoordinates] -> Get [Either LoadError Triangle]
loadTriangles 0 _ = return []
loadTriangles remaining_tris tex_coords = do
  vert_indices@(vi1, vi2, vi3) <- liftM3 (,,) (fromIntegral <$> getWord16le)
                                              (fromIntegral <$> getWord16le)
                                              (fromIntegral <$> getWord16le)
  tex_coord_indicies@(tc1, tc2, tc3) <- liftM3 (,,)
                                               (fromIntegral <$> getWord16le)
                                               (fromIntegral <$> getWord16le)
                                               (fromIntegral <$> getWord16le)
  let selected_tex_coords_n = if all (< length tex_coords) [tc1, tc2, tc3]
                              then Just (tex_coords !! tc1, tex_coords !! tc2,
                                         tex_coords !! tc3)
                              else Nothing

  remaining <- getRemainingLazyByteString
  let rest = runGet (loadTriangles (remaining_tris - 1) tex_coords) remaining

  case selected_tex_coords_n of
    Just selected_tex_coords ->
      return $ Right Triangle {
          triVertexIndices = vert_indices,
          triNormalIndices = Just vert_indices,
          triTextureCoordinateIndices = Just tex_coord_indicies
        }:rest
    Nothing -> return $ Left TextureCoordinatesNotFound:rest

loadTextures :: Int -> (Int, Int) -> Get [Texture]
loadTextures 0 _ = return []
loadTextures remaining_textures skin_size = do
  name <- bytesToString . takeWhile (/= 0) <$> mapM (const getWord8) [1..64]

  remaining <- getRemainingLazyByteString
  let rest = runGet (loadTextures (remaining_textures - 1) skin_size) remaining

  return $ Texture {
      texContents = (TexturePath name),
      texSize = skin_size
    }:rest
