{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CabalHoogle.Process
  ( -- * Inputs
    File
  , Directory
  , Argument
  , EnvKey
  , EnvValue
  , Process(..)

    -- * Outputs
  , Pass(..)
  , PassErr(..)
  , PassErrAnnihilate(..)
  , Clean(..)
  , Hush(..)
  , Out(..)
  , Err(..)
  , OutErr(..)
  , OutErrCode(..)
  , renderOutErrCode

    -- * Errors
  , ProcessError(..)
  , ExitStatus
  , ExitCode(..)
  , renderProcessError

    -- * Running Processes
  , ProcessResult(..)
  , call
  , call_
  , callFrom
  , callFrom_
  , capture
  , exec
  , execFrom

    -- * Internal (exported for testing)
  , cleanLines
  ) where

import           Control.Concurrent.Async (Async, async, waitCatch)
import           Control.Exception (IOException, SomeException, toException)
import           Control.Monad (MonadFail (..))
import           Control.Monad.Catch (MonadCatch (..), bracket_, handle)
import           Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, left, newEitherT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           CabalHoogle.IO (setCurrentDirectory)
import           CabalHoogle.P
import           CabalHoogle.Path (Directory, File)

import           System.Exit (ExitCode (..))
import           System.IO (BufferMode (..), FilePath, Handle, IO)
import qualified System.IO as IO
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as Posix
import qualified System.Process as Process
import qualified System.Process.Internals as ProcessInternals

------------------------------------------------------------------------

type Argument = Text
type EnvKey   = Text
type EnvValue = Text

data Process = Process
  { processCommand     :: File
  , processArguments   :: [Argument]
  , processDirectory   :: Maybe Directory
  , processEnvironment :: Maybe (Map EnvKey EnvValue)
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

-- | Pass @stdout@ and @stderr@ through to the console.
data Pass =
  Pass
  deriving (Eq, Ord, Show)

-- | Pass @stdout@ and @stderr@ through to the console, but redirect @stdout@ > @stderr.
data PassErr =
  PassErr
  deriving (Eq, Ord, Show)

-- | Pass @stdout@ and @stderr@ through to the console, but redirect @stdout@ > @stderr; also kill *everything* on Ctrl-C.
data PassErrAnnihilate =
  PassErrAnnihilate
  deriving (Eq, Ord, Show)

-- | Pass @stdout@ and @stderr@ through to the console, but process control
--   characters (such as \b, \r) prior to emitting each line of output.
data Clean =
  Clean
  deriving (Eq, Ord, Show)

-- | Capture @stdout@ and @stderr@ but ignore them.
data Hush =
  Hush
  deriving (Eq, Ord, Show)

-- | Capture @stdout@ and pass @stderr@ through to the console.
newtype Out a =
  Out {
      unOut :: a
    } deriving (Eq, Ord, Show, Functor)

-- | Capture @stderr@ and pass @stdout@ through to the console.
newtype Err a =
  Err {
      unErr :: a
    } deriving (Eq, Ord, Show, Functor)

-- | Capture both @stdout@ and @stderr@.
data OutErr a =
  OutErr !a !a
  deriving (Eq, Ord, Show, Functor)

-- | Capture @stdout@, @stderr@ and the 'ExitCode'.
--   /This never causes a @ProcessFailure@/
data OutErrCode a =
  OutErrCode !a !a !ExitCode
  deriving (Eq, Ord, Show, Functor)

renderOutErrCode :: OutErrCode Text -> Text
renderOutErrCode (OutErrCode out0 err0 exit) =
  let
    out =
      T.strip out0

    err =
      T.strip err0

    output =
     out <> (if T.null out then "" else "\n") <>
     err
  in
    case exit of
      ExitFailure code ->
        "Process failed with exit code: " <> T.pack (show code) <> "\n" <>
        output
      ExitSuccess ->
        "Process finished successfully:\n" <>
        output

-- ------------------------------------------------------------------------

type ExitStatus =
  Int

data ProcessError =
    ProcessFailure !Process !ExitStatus
  | ProcessException !Process !SomeException
    deriving (Show)

renderProcessError :: ProcessError -> Text
renderProcessError = \case
  ProcessFailure p code ->
    "Process failed: " <> T.intercalate " " (processCommand p : processArguments p) <>
    " (exit code: " <> T.pack (show code) <> ")"

  ProcessException p ex ->
    "Process failed: " <> T.intercalate " " (processCommand p : processArguments p) <>
    "\n" <> T.pack (show ex)

------------------------------------------------------------------------

createProcess :: MonadIO m => Process.CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
createProcess = liftIO . Process.createProcess

-- Spawn a new process, and if we get a ctrl-c, make absolutely sure everything we started is finished.
createProcessAnnihilate :: (MonadIO m, MonadCatch m) => Process.CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
createProcessAnnihilate cp = do
  (a, b, c, ph) <- createProcess cp { Process.create_group = True }
  pgid <- tryProcessGroupOfProcessHandle ph
  maybe (pure ()) installInterruptHandler pgid
  pure (a, b, c, ph)

tryPosixPidOfProcessHandle :: MonadIO m => Process.ProcessHandle -> m (Maybe Posix.ProcessID)
tryPosixPidOfProcessHandle ph =
  liftIO $ ProcessInternals.withProcessHandle ph $
   \case
    ProcessInternals.OpenHandle i   -> pure $ Just i
    ProcessInternals.ClosedHandle _ -> pure $ Nothing
    ProcessInternals.OpenExtHandle i _ -> pure $ Just i

tryProcessGroupOfProcessHandle :: (MonadIO m, MonadCatch m) => Process.ProcessHandle -> m (Maybe Posix.ProcessGroupID)
tryProcessGroupOfProcessHandle ph = do
  pid <- tryPosixPidOfProcessHandle ph
  case pid of
   Nothing -> pure Nothing
   Just h  -> handle ignoreIOE $ do
    pgid <- liftIO (Posix.getProcessGroupIDOf h)
    pure $ Just pgid
 where
  ignoreIOE (_ :: IOException) = pure Nothing

installInterruptHandler :: MonadIO m => Posix.ProcessGroupID -> m ()
installInterruptHandler pgid = do
  _ <- liftIO $ Signals.installHandler Signals.keyboardSignal (Signals.Catch $ Signals.signalProcessGroup Signals.keyboardTermination pgid) Nothing
  pure ()


class ProcessResult a where
  callProcess :: (Functor m, MonadIO m, MonadCatch m, MonadFail m)
              => Process -> EitherT ProcessError m a

instance ProcessResult Pass where
  callProcess p = withProcess p $ do
    let cp = fromProcess p

    (Nothing, Nothing, Nothing, pid) <- createProcess cp

    code <- liftIO (Process.waitForProcess pid)
    pure (code, Pass)

instance ProcessResult PassErr where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.UseHandle IO.stderr }

    (Nothing, Nothing, Nothing, pid) <- createProcess cp

    code <- liftIO (Process.waitForProcess pid)
    pure (code, PassErr)

instance ProcessResult PassErrAnnihilate where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.UseHandle IO.stderr }

    (Nothing, Nothing, Nothing, pid) <- createProcessAnnihilate cp

    code <- liftIO (Process.waitForProcess pid)
    pure (code, PassErrAnnihilate)

instance ProcessResult (Out ByteString) where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.CreatePipe }

    (Nothing, Just hOut, Nothing, pid) <- createProcess cp

    out  <- liftIO (B.hGetContents hOut)
    code <- liftIO (Process.waitForProcess pid)

    pure (code, Out out)

instance ProcessResult (Err ByteString) where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_err = Process.CreatePipe }

    (Nothing, Nothing, Just hErr, pid) <- createProcess cp

    err  <- liftIO (B.hGetContents hErr)
    code <- liftIO (Process.waitForProcess pid)

    pure (code, Err err)

instance ProcessResult (OutErr ByteString) where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.CreatePipe
                             , Process.std_err = Process.CreatePipe }

    (Nothing, Just hOut, Just hErr, pid) <- createProcess cp

    asyncOut <- liftIO (async (B.hGetContents hOut))
    asyncErr <- liftIO (async (B.hGetContents hErr))

    out  <- waitCatchE p asyncOut
    err  <- waitCatchE p asyncErr
    code <- liftIO (Process.waitForProcess pid)

    pure (code, OutErr out err)

instance ProcessResult (OutErrCode ByteString) where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.CreatePipe
                             , Process.std_err = Process.CreatePipe }

    (Nothing, Just hOut, Just hErr, pid) <- createProcess cp

    asyncOut <- liftIO (async (B.hGetContents hOut))
    asyncErr <- liftIO (async (B.hGetContents hErr))

    out  <- waitCatchE p asyncOut
    err  <- waitCatchE p asyncErr
    code <- liftIO (Process.waitForProcess pid)

    pure (ExitSuccess, OutErrCode out err code)

instance ProcessResult Hush where
  callProcess p = do
    OutErr (_ :: ByteString) (_ :: ByteString) <- callProcess p
    pure Hush

instance ProcessResult Clean where
  callProcess p = withProcess p $ do
    let cp = (fromProcess p) { Process.std_out = Process.CreatePipe
                             , Process.std_err = Process.CreatePipe }

    (Nothing, Just hOut, Just hErr, pid) <- createProcess cp

    asyncOut <- liftIO (async (clean hOut IO.stdout))
    asyncErr <- liftIO (async (clean hErr IO.stderr))

    ()   <- waitCatchE p asyncOut
    ()   <- waitCatchE p asyncErr
    code <- liftIO (Process.waitForProcess pid)

    pure (code, Clean)

instance ProcessResult (Out Text) where
  callProcess p = fmap T.decodeUtf8 <$> callProcess p

instance ProcessResult (Err Text) where
  callProcess p = fmap T.decodeUtf8 <$> callProcess p

instance ProcessResult (OutErr Text) where
  callProcess p = fmap T.decodeUtf8 <$> callProcess p

instance ProcessResult (OutErrCode Text) where
  callProcess p = fmap T.decodeUtf8 <$> callProcess p

------------------------------------------------------------------------

-- | Call a command with arguments.
--
call :: (ProcessResult a, Functor m, MonadIO m, MonadCatch m, MonadFail m)
     => (ProcessError -> e)
     -> File
     -> [Argument]
     -> EitherT e m a

call up cmd args = firstEitherT up (callProcess process)
  where
    process = Process { processCommand     = cmd
                      , processArguments   = args
                      , processDirectory   = Nothing
                      , processEnvironment = Nothing }

-- | Call a command with arguments, passing the output through to stdout/stderr.
--
call_ :: (Functor m, MonadIO m, MonadCatch m, MonadFail m)
      => (ProcessError -> e)
      -> File
      -> [Argument]
      -> EitherT e m ()

call_ up cmd args = do
  Pass <- call up cmd args
  pure ()

-- | Call a command with arguments from inside a working directory.
--
callFrom :: (ProcessResult a, Functor m, MonadIO m, MonadCatch m, MonadFail m)
         => (ProcessError -> e)
         -> Directory
         -> File
         -> [Argument]
         -> EitherT e m a

callFrom up dir cmd args = firstEitherT up (callProcess process)
  where
    process = Process { processCommand     = cmd
                      , processArguments   = args
                      , processDirectory   = Just dir
                      , processEnvironment = Nothing }

-- | Call a command with arguments from inside a working directory.
--
callFrom_ :: (Functor m, MonadIO m, MonadCatch m, MonadFail m)
          => (ProcessError -> e)
          -> Directory
          -> File
          -> [Argument]
          -> EitherT e m ()

callFrom_ up dir cmd args = do
  Pass <- callFrom up dir cmd args
  pure ()

-- | Capture the output of a process when it fails.
--
capture ::
  (OutErrCode Text -> x) ->
  EitherT x IO (OutErrCode Text) ->
  EitherT x IO ()
capture fromOutput p = do
  output@(OutErrCode _ _ code) <- p
  case code of
    ExitFailure _ ->
      left $ fromOutput output
    ExitSuccess ->
      pure ()

------------------------------------------------------------------------

-- | Execute a process, this call never returns.
--
execProcess :: (MonadIO m, MonadCatch m) => Process -> EitherT ProcessError m a
execProcess p = handleIO p $ do
    case processDirectory p of
      Nothing  -> pure ()
      Just dir -> setCurrentDirectory dir
    liftIO (Posix.executeFile cmd True args env)
  where
    (cmd, args, _, env) = fromProcess' p

-- | Execute a command with arguments, this call never returns.
--
exec :: (Functor m, MonadIO m, MonadCatch m)
     => (ProcessError -> e)
     -> File
     -> [Argument]
     -> EitherT e m a

exec up cmd args = firstEitherT up (execProcess process)
  where
    process = Process { processCommand     = cmd
                      , processArguments   = args
                      , processDirectory   = Nothing
                      , processEnvironment = Nothing }

-- | Execute a command with arguments, this call never returns.
--
execFrom :: (Functor m, MonadIO m, MonadCatch m)
         => (ProcessError -> e)
         -> Directory
         -> File
         -> [Argument]
         -> EitherT e m a

execFrom up dir cmd args = firstEitherT up (execProcess process)
  where
    process = Process { processCommand     = cmd
                      , processArguments   = args
                      , processDirectory   = Just dir
                      , processEnvironment = Nothing }

------------------------------------------------------------------------

withProcess :: (MonadIO m, MonadCatch m)
            => Process
            -> EitherT ProcessError m (ExitCode, a)
            -> EitherT ProcessError m a

withProcess p io = handleIO p $ do
  (code, result) <- io
  case code of
    ExitSuccess   -> pure result
    ExitFailure x -> hoistEither (Left (ProcessFailure p x))

fromProcess :: Process -> Process.CreateProcess
fromProcess p = Process.CreateProcess
    { Process.cmdspec            = Process.RawCommand cmd args
    , Process.cwd                = cwd
    , Process.env                = env
    , Process.std_in             = Process.Inherit
    , Process.std_out            = Process.Inherit
    , Process.std_err            = Process.Inherit
    , Process.close_fds          = False
    , Process.create_group       = False
    , Process.delegate_ctlc      = False
    , Process.detach_console     = False
    , Process.create_new_console = False
    , Process.new_session        = False
    , Process.child_group        = Nothing
    , Process.child_user         = Nothing
    , Process.use_process_jobs   = False -- Ignored on POSIX systems so default it to false.
    }
  where
    (cmd, args, cwd, env) = fromProcess' p

fromProcess' :: Process -> (FilePath, [String], Maybe FilePath, Maybe [(String, String)])
fromProcess' p = (cmd, args, cwd, env)
  where
    cmd  = T.unpack (processCommand p)
    args = fmap T.unpack (processArguments p)
    cwd  = fmap T.unpack (processDirectory p)

    env  = fmap (fmap (bimap T.unpack T.unpack) . Map.toList)
                (processEnvironment p)

------------------------------------------------------------------------

handleIO :: MonadCatch m => Process -> EitherT ProcessError m a -> EitherT ProcessError m a
handleIO p =
  let fromIO = toException :: IOException -> SomeException
  in handle (hoistEither . Left . ProcessException p . fromIO)

waitCatchE :: (Functor m, MonadIO m) => Process -> Async a -> EitherT ProcessError m a
waitCatchE p = firstEitherT (ProcessException p) . newEitherT . liftIO . waitCatch

------------------------------------------------------------------------

clean :: Handle -> Handle -> IO ()
clean input output = do
  ibuf <- IO.hGetBuffering input
  obuf <- IO.hGetBuffering output

  let setLineBuffering = do
        IO.hSetBuffering input  LineBuffering
        IO.hSetBuffering output LineBuffering

      ignoreIOE (_ :: IOException) = pure ()

      -- the handles may be closed by the time we
      -- try to reset the buffer mode, so we need
      -- to catch exceptions
      resetBuffering = do
        handle ignoreIOE (IO.hSetBuffering input  ibuf)
        handle ignoreIOE (IO.hSetBuffering output obuf)

  bracket_ setLineBuffering resetBuffering $ do
    xs <- IO.hGetContents input
    IO.hPutStr output (cleanLines [] xs)


cleanLines :: [Char] -- ^ current line
           -> [Char] -- ^ input
           -> [Char] -- ^ output

-- backspace - delete previous character
cleanLines (_ : line) ('\b' : xs) = cleanLines line xs
cleanLines []         ('\b' : xs) = cleanLines []   xs

-- carriage return - delete the whole line
cleanLines _          ('\r' : xs) = cleanLines []   xs

-- line feed - emit the current line
cleanLines line       ('\n' : xs) = reverse ('\n' : line) <> cleanLines [] xs

-- normal character - add to current line
cleanLines line       (x    : xs) = cleanLines (x : line) xs

-- end of stream - emit the current line
cleanLines line       []          = line
