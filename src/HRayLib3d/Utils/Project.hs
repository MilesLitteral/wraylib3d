{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, DeriveGeneric, DeriveAnyClass, DataKinds #-}

module HRayLib3d.Utils.Project where

        import GHC.Generics  (Generic)
        import Data.Text     (Text, pack)
        import Data.Monoid   (getSum)
        import Data.Aeson    (ToJSON, FromJSON)
        import GHC.Conc.Sync (unsafeIOToSTM) -- WARNING: This is only for testing STM functions, and even then, to experiment
        import HRayLib3d.Network.Database
        import Database.PostgreSQL.Simple
        import Control.Concurrent.STM.TMVar (TMVar(..))
        import qualified Data.ByteString         as BS
        import qualified Data.Map                as S
        import qualified Control.Applicative     as S
        import qualified Control.Concurrent.STM  as STM
        -- import qualified HRayLib3d.Utils.Database as DB

        -- FileSystem Functions
        type ProjectId     = Int
        type RealmValueSTM = STM.STM (TMVar Int)

        data Project     
                = Project      ProjectId WRLPFile
                | ProjectGroup Text      [Project] 
                deriving(Generic, ToJSON, FromJSON, Show, Eq)

        data WRLFileType = 
                NULL_FILE       -- An Empty File (like null subset) 
                |FOLDER         -- Folders
                |WRL_PROJECT    -- .wrlp files
                |WRL_CONFIG     -- a catch all for internal config types (except for .lc) ie: .yaml, .cabal, .djinni, etc
                |TEXTURE_ASSET  -- a catch all for Texture files (.jpg, .png, .dds, .bmp, etc)
                |QUAKE3_SHADER  -- Quake 3's Shader Format (Pipeline Command Format)
                |GLSL_SHADER    -- OpenGL Shading Language Program (.glsl)
                |METAL_SHADER   -- Apple Metal Shader Language (.metal)
                |METAL_LIB      -- Apple Metal Shader Language (.metallib)
                |ALEMBIC        -- Alembic File Format (.abc)
                |MATERIAL       -- Materials (.mat)
                |ASSET_BUNDLE   -- Compiled Asset  Collections (.assetBundle)
                |SHADER_CACHE   -- Compiled Shader Collections (.shaderCache)
                |GLB_ASSET      -- OpenGL Binary Format (.glb)
                |GLTF_ASSET     -- OpenGL Transmission Format (.gltf)
                |BSP_ASSET      -- Binary Space Partition (.bsp)
                |OGG_ASSET      -- OGG Audio File Format (.ogg)
                |WAV_ASSET      -- Waveform Audio File Format (.wav)
                |MP3_ASSET      -- MPEG-1 Audio Layer 3 (.mp3)
                |TEXT_FILE      -- Plain Text (.txt)
                |HASKELL_FILE   -- Haskell Script (.hs)
                |CPP_FILE       -- C++ Script/Header (.cpp/.cc/.h/.hpp)
                |GEMFILE        -- Ruby Gem   (.gem)
                |RUBY_FILE      -- Ruby Script (.rb)
                |LC_FILE        -- LambdaCube/Local Cache Script (.lc)
                |GENERIC_FILE   -- any file type not explicitly defined in this list
                deriving (Generic, Eq, ToJSON, FromJSON, Show, Enum)

        newtype WRLPFile = WRLPFile { wType :: [(Text, WRLFileType)]} deriving (Generic, Eq, ToJSON, FromJSON, Show)

        instance Semigroup WRLPFile where
                (WRLPFile w1) <> (WRLPFile w2) = WRLPFile (w1 ++ w2)

        instance Monoid WRLPFile where
                mempty = WRLPFile [(pack "NULL", NULL_FILE)]

        appendProjectToGroup :: Project -> [Project] -> Project
        appendProjectToGroup project projects = do
                let pg = [project]
                ProjectGroup (pack $ show $ id projects) (projects ++ pg) 
        
        -- DB Reporting, STM Version (Has Issues)
        openRealmSTM :: BS.ByteString -> Query  -> RealmValueSTM
        openRealmSTM libpq query    = do 
            rb <- unsafeIOToSTM $ sendDBQuery libpq query
            STM.newTMVar (rb)
            -- STM.putTMVar value rb
            -- STM.takeTMVar 

        runDLServiceSTM :: BS.ByteString -> Query  -> IO ()
        runDLServiceSTM libpq query = do
            dbSTM   <- STM.atomically  (openRealmSTM libpq query)
            output  <- STM.atomically  (STM.takeTMVar  dbSTM)
            print output
