{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Core.BuildBundler (buildWin32) where

    --windows support only for now
    import Development.NSIS

    buildWin32 :: IO()
    buildWin32 = writeFile "example1.nsi" $ nsis $ do
        name       "HRayLib3d-Game"            -- The name of the installer
        outFile    "HRayLib3d.exe"        -- Where to produce the installer
        installDir "$DESKTOP/HRayLib3d"   -- The default installation directory
        requestExecutionLevel User       -- Request application privileges for Windows Vista
        -- Pages to display
        page Directory                   -- Pick where to install
        page InstFiles                   -- Give a progress bar while installing
        -- Groups fo files to install
        section "" [] $ do
            setOutPath "$INSTDIR"        -- Where to install files in this section
            file [] "app.hs"        -- File to put into this section