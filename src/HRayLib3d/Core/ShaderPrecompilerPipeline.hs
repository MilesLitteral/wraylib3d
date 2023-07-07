-- https://stackoverflow.com/questions/20443560/how-to-practically-ship-glsl-shaders-with-your-c-software

module HRayLib3d.Core.ShaderPrecompilerPipeline where

    import MegaStore              ( MegaStore(MegaStore) )
    import System.Directory       ( createDirectoryIfMissing, getDirectoryContents )
    import Codec.Compression.GZip ( compress )

    import Data.Binary ( encode )
    import qualified Data.Text as T
    import qualified Data.ByteString      as BS
    import qualified Data.ByteString.Lazy as BL

    type ShaderCache = MegaStore

    saveCache :: String -> ShaderCache -> IO ()
    saveCache name store = BL.writeFile (name ++ ".shaderCache") (compress $ encode store)

    setShaderForPreCompC :: String -> IO String
    setShaderForPreCompC scriptBody = return $ show $ "R\"(" ++ scriptBody ++ ")\""
    
    -- and then import it as a string like this:
    --const std::string vs_source = "#include shaderName.vs";
    vs_source :: String -> String
    vs_source shaderName = "#include" ++ shaderName ++ ".vs";
    
    createShaderCache :: [FilePath] -> IO ShaderCache
    createShaderCache cache = do
        bytes <- mapM (BS.fromFilePath) cache
        let compiledCache   = zip (map T.pack cache) bytes
        return $ MegaStore compiledCache

    writeShaderCache :: String -> ShaderCache -> IO ()
    writeShaderCache cacheName store = do
        createDirectoryIfMissing False "./shaderCache/"
        saveCache ("./shaderCache/" ++ cacheName)  store
    
    -- Shader Cache Sources
    glesShaderCache   :: IO [FilePath]
    glesShaderCache   = getDirectoryContents  "./shaders/GLES/"

    glslShaderCache   :: IO [FilePath]
    glslShaderCache   = getDirectoryContents  "./shaders/GLSL/"

    vulkanShaderCache :: IO [FilePath]
    vulkanShaderCache = getDirectoryContents  "./shaders/Vulkan/"

    hlslShaderCache   :: IO [FilePath]
    hlslShaderCache   = getDirectoryContents  "./shaders/HLSL/"

    metalShaderCache  :: IO [FilePath]
    metalShaderCache  = getDirectoryContents  "./shaders/Metal/"