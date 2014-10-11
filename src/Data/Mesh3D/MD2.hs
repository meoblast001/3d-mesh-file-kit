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

import Data.Binary.Get
import Data.ByteString.Lazy
import Data.Mesh3D

load :: ByteString -> Maybe Mesh3D
load data_in =
  let main_monad = do
        ident <- getWord32le
        version <- getWord32le

        skin_width <- getWord32le
        skin_height <- getWord32le

        frame_size <- getWord32le

        num_skins <- getWord32le
        num_verts <- getWord32le
        num_tex_coords <- getWord32le
        num_tris <- getWord32le
        num_glcmds <- getWord32le
        num_frames <- getWord32le

        offset_skins <- getWord32le
        offset_tex_coords <- getWord32le
        offset_tris <- getWord32le
        offset_frames <- getWord32le
        offset_gl_commands <- getWord32le
        offset_end <- getWord32le

        if ident == 844121161 then -- "IDP2" as integer.
          return $ Just Mesh3D {
              textureSize = (fromIntegral skin_width, fromIntegral skin_height)
            }
        else
          return Nothing
  in runGet main_monad data_in
