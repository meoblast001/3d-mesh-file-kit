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

load :: LBS.ByteString -> Maybe Mesh3D
load data_in =
  let
      load_frames 0 = return []
      load_frames remaining_frames = do
        scale <- liftM3 (,,) getFloat32le getFloat32le getFloat32le
        translate <- liftM3 (,,) getFloat32le getFloat32le getFloat32le
        name <- bytesToString . (takeWhile (/= 0)) <$>
                mapM (const getWord8) [1..8]

        remaining <- getRemainingLazyByteString
        return $ Frame { name = name }:
                (runGet (load_frames (remaining_frames - 1)) remaining)
      main_func = do
        ident <- getWord32le
        version <- getWord32le

        skin_width <- getWord32le
        skin_height <- getWord32le

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

        if ident == 844121161 then -- "IDP2" as integer.
          return $ Just Mesh3D {
              textureSize = (fromIntegral skin_width, fromIntegral skin_height),
              frames = runGet (load_frames num_frames)
                              (LBS.drop offset_frames data_in)
            }
        else
          return Nothing
  in runGet main_func data_in
