{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeSynonymInstances, OverloadedStrings,  FlexibleInstances, UndecidableInstances, LambdaCase #-}

module HRayLib3d.ScriptEngine.Internal where
  import Control.Monad
  import Data.Aeson
  import Data.Aeson.Key (coercionToText, fromText, toText)
  import qualified Data.Aeson.KeyMap as KM
  import qualified Data.ByteString.Char8 as BS
  import Data.IORef
  import Data.Either.Extra
  import Data.Scientific
  import Data.Type.Coercion (coerceWith)
  import qualified Data.Text as T
  import qualified Data.Vector as V
  import qualified Data.Text.Encoding as T

  import Foreign.C
  import Foreign hiding (void)
  import System.IO
  import System.Process 
  import Control.Applicative
  import Control.Concurrent
  import Control.Exception.Base
  import Control.Monad
  import Control.Exception
  import Control.Concurrent.STM
  import HRayLib3d.ScriptEngine.Bindings

  -- Ruby must be installed, a bit buggy it seems
  runRubyInterpreter :: IO () --Handle
  runRubyInterpreter = do
      (Just  hin, Just hout, _, _) <- createProcess (proc "ruby" ["./script-engine/hello.rb"]){ cwd = Just "./", std_in = CreatePipe, std_out = CreatePipe }
      -- RubyInterpreterProcess hin hout
      print hout

  -- -- Foreign Ruby Side Representation of Interface
  -- -- | All those function types can be used to register functions to the Ruby
  -- -- runtime. Please note that the first argument is always set (it is
  -- -- \"self\"). For this reason, there is no @RubyFunction0@ type.
  type RubyFunction1 = FRValue -> IO FRValue
  type RubyFunction2 = FRValue -> FRValue -> IO FRValue
  type RubyFunction3 = FRValue -> FRValue -> FRValue -> IO FRValue
  type RubyFunction4 = FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
  type RubyFunction5 = FRValue -> FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
  type NoOutput      = TMVar (Maybe RubyError)

  foreign import ccall "wrapper" mkRegisteredRubyFunction1 :: RubyFunction1 -> IO (FunPtr RubyFunction1)
  foreign import ccall "wrapper" mkRegisteredRubyFunction2 :: RubyFunction2 -> IO (FunPtr RubyFunction2)
  foreign import ccall "wrapper" mkRegisteredRubyFunction3 :: RubyFunction3 -> IO (FunPtr RubyFunction3)
  foreign import ccall "wrapper" mkRegisteredRubyFunction4 :: RubyFunction4 -> IO (FunPtr RubyFunction4)
  foreign import ccall "wrapper" mkRegisteredRubyFunction5 :: RubyFunction5 -> IO (FunPtr RubyFunction5)

  -- | This is actually a newtype around a 'TQueue'.
  newtype RubyInterpreter = RubyInterpreter (TQueue IMessage)
  type    RubyValueSTM    = STM (TMVar (Either RubyError FRValue))

  data IMessage = MsgStop
                | MsgLoadFile !FilePath !NoOutput
                | RegisterGlobalFunction1 !String !RubyFunction1 !NoOutput
                | RegisterGlobalFunction2 !String !RubyFunction2 !NoOutput
                | RegisterGlobalFunction3 !String !RubyFunction3 !NoOutput
                | RegisterGlobalFunction4 !String !RubyFunction4 !NoOutput
                | RegisterGlobalFunction5 !String !RubyFunction5 !NoOutput
                | MakeSafe !(IO ()) !NoOutput

  data RubyError = Stack String String
                | WithOutput String FRValue
                | OtherError String
                deriving Show

  class ForeignToRuby   a where 
    toRubyF   :: a -> IO FRValue
  
  class ForeignFromRuby a where
    fromRubyF :: FRValue -> IO (Either String a)

  instance ForeignFromRuby a => ForeignFromRuby [a] where
      fromRubyF v = do
          t <- rtype v
          case t of
              RBuiltin RARRAY -> fromRubyArray v
              _ -> return $ Left ("not an array! " ++ show t)
      
  instance ForeignToRuby a => ForeignToRuby [a] where
      toRubyF lst = do
          vals <- mapM toRubyF lst
          Foreign.withArray vals (rb_ary_new4 (fromIntegral (length lst)))

  instance ForeignFromRuby BS.ByteString where
      fromRubyF v = do
          t <- rtype v
          case t of
              RBuiltin RSTRING -> do
                  pv <- new v
                  cstr <- c_rb_string_value_cstr pv
                  free pv
                  fmap Right (BS.packCString cstr)
              RSymbol -> fmap Right (rb_id2name (sym2id v) >>= BS.packCString)
              _ -> return (Left ("Expected a string, not " ++ show t))

  instance ForeignToRuby BS.ByteString where
      toRubyF s = BS.useAsCString s c_rb_str_new2

  instance ForeignToRuby  T.Text where
      toRubyF = toRubyF . T.encodeUtf8

  instance ForeignToRuby Double where
      toRubyF = newFloat

  instance ForeignFromRuby Double where
      fromRubyF = fmap Right . num2dbl

  instance ForeignFromRuby Integer where
      fromRubyF = fromRubyIntegral

  instance ForeignToRuby Integer where
      toRubyF = toRubyIntegral

  instance ForeignFromRuby Int where
      fromRubyF = fromRubyIntegral

  instance ForeignToRuby Int where
      toRubyF = toRubyIntegral

  instance ForeignFromRuby Key where
      fromRubyF = fmap (fmap fromText) . fromRubyF @T.Text

  instance ForeignFromRuby T.Text where
    fromRubyF = fmap (fmap T.decodeUtf8) . fromRubyF

  fromRubyIntegral :: Integral n => FRValue -> IO (Either String n)
  fromRubyIntegral = fmap (Right . fromIntegral) . num2long
  
  toRubyIntegral :: Integral n => n -> IO FRValue
  toRubyIntegral = int2num . fromIntegral
  
  fromRubyArray :: ForeignFromRuby a => FRValue -> IO (Either String [a])
  fromRubyArray v = do
    nbelems <- arrayLength v
    fmap sequence (forM [0 .. (nbelems - 1)] (rb_ary_entry v >=> fromRubyF))
  
  valueOrError :: ForeignFromRuby a => Either RubyError a -> a
  valueOrError ve = case ve of 
                      Left  err -> error $ show err
                      Right val -> val

  rvalueOrError :: Either RubyError FRValue -> FRValue
  rvalueOrError ve = case ve of 
                      Left  err -> error $ show err
                      Right val -> val
    
  -- | Runs a Ruby method, capturing errors.
  -- | Arguments. Please note that the maximum number of arguments is 16.
  -- | Returns either an error message / value couple, or the value returned by the function.
  safeFunCall' :: FRValue -> String ->[FRValue] ->  IO (Either (String, FRValue) FRValue)
  safeFunCall' recv methodname args
    | length args > 16 = pure $ Left ("too many arguments", rbNil)
    | otherwise =
    rb_intern methodname >>= \methodid ->
      with (ShimDispatch recv methodid args) $ \dispatch ->
        with 0 $ \pstatus -> do
          o <- c_rb_protect safeCallback (castPtr dispatch) pstatus
          status <- peek pstatus
          if status == 0
            then pure $ Right o
            else do
              err <- showErrorStack
              pure $ Left (err, o)

  -- | A safe version of the corresponding "Foreign.Ruby.Helper" function.
  safeFunCall :: RubyInterpreter -> FRValue -> String -> [FRValue] -> IO (Either RubyError FRValue)
  safeFunCall int receiver methodName args = do
    o <- makeSafe int $ safeFunCall' receiver methodName args
    case o of
      Left x -> return (Left x)
      Right (Right v) -> return (Right v)
      Right (Left (s, v)) -> return (Left (WithOutput s v))

  -- | Gets a Ruby class, capturing errors.
  safeGetClass :: String -> IO (Either (String, FRValue) FRValue)
  safeGetClass s =
    withCString s $ \cs ->
      with 0 $ \pstatus -> do
        o <- c_rb_protect getRubyCObjectCallback (castPtr cs) pstatus
        status <- peek pstatus
        if status == 0
          then pure $ Right o
          else do
            err <- showErrorStack
            pure $ Left (err, o)

  -- | Runs a Ruby singleton method, capturing errors.
  safeMethodCall :: String -> String -> [FRValue] -> IO (Either (String, FRValue) FRValue)
  safeMethodCall classname methodname args = do
    erecv <- safeGetClass classname
    case erecv of
      Right recv -> safeFunCall' recv methodname args
      Left  err  -> pure $ Left err

  -- | Gives a (multiline) error friendly string representation of the last
  -- error.
  showErrorStack :: IO String
  showErrorStack = do
    runtimeerror <- rb_gv_get "$!"
    m <-
      if runtimeerror == rbNil
        then return "Unknown runtime error"
        else do
          message <- rb_intern "message"
          fmap (either T.pack id) (rb_funcall runtimeerror message [] >>= fromRubyF)
    rbt <- rb_gv_get "$@"
    bt <-
      if rbt == rbNil
        then return []
        else fmap (either (const []) id) (fromRubyF rbt)
    return (T.unpack (T.unlines (m : bt)))

  go :: TQueue IMessage -> IO ()
  go q = do
      let continue = return False
          stop     = return True
          runNoOutput :: NoOutput -> IO () -> IO Bool
          runNoOutput no a = do
              try a >>= atomically . putTMVar no . either (\e -> Just $ OtherError $ show (e :: SomeException))
                                                          (const Nothing)
              continue
          runReturns0 :: NoOutput -> IO Int -> String -> IO Bool
          runReturns0 no a errmsg  = do
              s <- try a
              case s of
                  Right 0 -> atomically (putTMVar no Nothing)
                  Right _ -> do
                      stack <- showErrorStack
                      atomically $ putTMVar no $ Just $ Stack errmsg stack
                  Left e -> atomically $ putTMVar no $ Just $ OtherError $ show (e :: SomeException)
              continue

      finished <- atomically (readTQueue q) >>= \case
          MsgStop -> stop
          MsgLoadFile fp mv -> runReturns0 mv (rb_load_protect fp 0)  ("Could not load " ++ fp)
          RegisterGlobalFunction1 fname f no -> runNoOutput no $ mkRegisteredRubyFunction1 f >>= \rf -> rb_define_global_function fname rf 0
          RegisterGlobalFunction2 fname f no -> runNoOutput no $ mkRegisteredRubyFunction2 f >>= \rf -> rb_define_global_function fname rf 1
          RegisterGlobalFunction3 fname f no -> runNoOutput no $ mkRegisteredRubyFunction3 f >>= \rf -> rb_define_global_function fname rf 2
          RegisterGlobalFunction4 fname f no -> runNoOutput no $ mkRegisteredRubyFunction4 f >>= \rf -> rb_define_global_function fname rf 3
          RegisterGlobalFunction5 fname f no -> runNoOutput no $ mkRegisteredRubyFunction5 f >>= \rf -> rb_define_global_function fname rf 4
          MakeSafe a no -> runNoOutput no a
      if finished
          then ruby_finalize
          else go q
   
  registerGlobalFunction1 :: RubyInterpreter -> String -> RubyFunction1 -> IO (Either RubyError ())
  registerGlobalFunction1 int fname f = runMessage_ int (RegisterGlobalFunction1 fname f)
  registerGlobalFunction2 :: RubyInterpreter -> String -> RubyFunction2 -> IO (Either RubyError ())
  registerGlobalFunction2 int fname f = runMessage_ int (RegisterGlobalFunction2 fname f)
  registerGlobalFunction3 :: RubyInterpreter -> String -> RubyFunction3 -> IO (Either RubyError ())
  registerGlobalFunction3 int fname f = runMessage_ int (RegisterGlobalFunction3 fname f)
  registerGlobalFunction4 :: RubyInterpreter -> String -> RubyFunction4 -> IO (Either RubyError ())
  registerGlobalFunction4 int fname f = runMessage_ int (RegisterGlobalFunction4 fname f)
  registerGlobalFunction5 :: RubyInterpreter -> String -> RubyFunction5 -> IO (Either RubyError ())
  registerGlobalFunction5 int fname f = runMessage_ int (RegisterGlobalFunction5 fname f)

  loadFile :: RubyInterpreter -> FilePath -> IO (Either RubyError ())
  loadFile int fp = runMessage_ int (MsgLoadFile fp)

  -- | Runs an arbitrary computation in the Ruby interpreter thread. This is
  -- useful if you want to embed calls from lower level functions. You still
  -- need to be careful about the GC's behavior.
  makeSafe :: RubyInterpreter -> IO a -> IO (Either RubyError a)
  makeSafe int a = do
      -- the IO a computation is embedded in an IO () computation, so that
      -- all is type safe
      mv <- newEmptyMVar
      let embedded = a >>= putMVar mv
      msg <- runMessage_ int (MakeSafe embedded)
      case msg of
          Right _ -> Right <$> takeMVar mv
          Left rr -> return (Left rr)

  runMessage_ :: RubyInterpreter -> (NoOutput -> IMessage) -> IO (Either RubyError ())
  runMessage_ (RubyInterpreter q) pm = do
      o <- newEmptyTMVarIO
      atomically (writeTQueue q (pm o))
      maybe (Right ()) Left <$> atomically (readTMVar o)

  -- | Initializes a Ruby interpreter. This should only be called once. It
  -- actually runs an internal server in a dedicated OS thread.
  startRubyInterpreter :: IO RubyInterpreter
  startRubyInterpreter = do
      q <- newTQueueIO
      void $ forkOS (ruby_initialization >> go q)
      return (RubyInterpreter q)

  {-| This is basically :

  > bracket startRubyInterpreter closeRubyInterpreter
  -}
  withRubyInterpreter :: (RubyInterpreter -> IO a) -> IO a
  withRubyInterpreter = bracket startRubyInterpreter closeRubyInterpreter

  -- | This will shut the internal server down.
  closeRubyInterpreter :: RubyInterpreter -> IO ()
  closeRubyInterpreter (RubyInterpreter q) = atomically (writeTQueue q MsgStop)

  -- | Sets the current GC operation. Please note that this could be modified
  -- from Ruby scripts.
  setGC ::
    -- | Set to `True` to enable GC, and to `False` to disable it.
    Bool ->
    IO (Either (String, FRValue) FRValue)
  setGC nw = do
    let method =
          if nw
            then "enable"
            else "disable"
    safeMethodCall "GC" method []

  -- | Runs the Ruby garbage collector.
  startGC :: IO ()
  startGC = Control.Monad.void (safeMethodCall "GC" "start" [])

  -- | Runs a computation with the Ruby GC disabled. Once the computation is over, GC will be re-enabled and the `startGC` function run.
  freezeGC :: RubyInterpreter -> IO a -> IO a
  freezeGC ri c = makeSafe ri (setGC False) *> c <* makeSafe ri (setGC True >> startGC)

  -- | Converts a Ruby value to some Haskell type..
  fromRubyFFI :: ForeignFromRuby a => RubyInterpreter -> FRValue -> IO (Either RubyError a)
  fromRubyFFI ri rv = either Left (either (Left . OtherError) Right) <$> makeSafe ri (fromRubyF rv)

  -- | Insert a value in the Ruby runtime. You must always use such
  -- a function and the resulting RValue ina 'freezeGC' call.
  toRubyFFI :: ForeignToRuby a => RubyInterpreter -> a -> IO (Either RubyError FRValue)
  toRubyFFI ri = makeSafe ri . toRubyF