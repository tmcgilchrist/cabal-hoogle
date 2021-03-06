{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CabalHoogle.Catch (
    Disaster(..)
  , bracketF
  , hushM
  , bracketEitherT'
  ) where

import           Control.Monad.Catch (Exception (..), MonadCatch (..), MonadMask (..), SomeException (..), catchAll,
                                      handle, throwM)
import           Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT)

import           Data.Typeable (typeOf)

import           GHC.Show (appPrec)

import           CabalHoogle.P

import           Text.Show (showChar, showParen)

-- | Newtype wrapper for 'SomeException' which provides an 'Eq' instance which
--   says no two disasters are equal. It also provides a 'Show' instance
--   containing only valid Haskell 98 tokens.
--
--   This is useful for embedding 'SomeException' in a sensible error type that
--   has equality for all the other cases.
newtype Disaster =
  Disaster {
      unDisaster :: SomeException
    }

instance Eq Disaster where
  (==) _ _ =
    False

instance Show Disaster where
  showsPrec prec (Disaster (SomeException e)) =
    showParen (prec > appPrec) $
      showString "Disaster " .
      showsPrec 11 (typeOf e) .
      showChar ' ' .
      showsPrec 11 (show e)

data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk <$> g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) <$> f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        pure b
      BracketOk b -> do
        z <- f a'
        pure $ either id (const b) z

-- | Run an action, turning exceptions which pass the predicate in to 'Nothing'
--   and re-throwing the rest.
--
--   Usage:
--   @
--   tryOpenFile :: MonadIO m => FilePath -> m (Maybe Handle)
--   tryOpenFile path =
--     liftIO . hushM isDoesNotExistError $
--       openBinaryFile path ReadMode
--   @
hushM :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> m (Maybe a)
hushM p m =
  let
    onError e =
      if p e then
        pure Nothing
      else
        throwM e
  in
    handle onError $
      fmap Just m

-- | Exception and `Left` safe version of bracketEitherT.
--
bracketEitherT' :: MonadMask m => EitherT e m a -> (a -> EitherT e m c) -> (a -> EitherT e m b) -> EitherT e m b
bracketEitherT' acquire release run =
  EitherT $ bracketF
    (runEitherT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        pure . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> pure $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        pure . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runEitherT (run r'))
