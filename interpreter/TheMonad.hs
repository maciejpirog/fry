{-# LANGUAGE DeriveDataTypeable #-}

module TheMonad where

{-

This module defines the monad that runs in the background
of the interpreter. Its purpose is twofold:

* It provides IO (via the IO monad of course!)

* It provides additional runtime abilities (statistics and
  debugging)

-}

import Control.Monad
import Control.Monad.Writer
import Control.Exception
import Data.Monoid
import Data.Typeable

data Stats = Stats
  {
    reds :: Int
  }

instance Monoid Stats where
  mempty = Stats 0
  mappend (Stats n) (Stats m) = Stats (n + m)

{- The following gives you the number of reductions,
   it is commented out for performance reasons.
   Uncomment to get stats

type TheMonad = WriterT Stats IO

io :: IO a -> TheMonad a
io = lift

data RuntimeException = RuntimeException String                
  deriving (Typeable)

instance Exception RuntimeException

instance Show RuntimeException where
  show (RuntimeException s) = s

err :: String -> TheMonad a
err = io . throw . RuntimeException

toIO :: TheMonad a -> IO a
toIO = fmap fst . runWriterT

stats :: TheMonad a -> IO Stats
stats = execWriterT

statRed :: TheMonad ()
statRed = tell (Stats 1)

-}

type TheMonad = IO

io :: IO a -> TheMonad a
io = id

data RuntimeException = RuntimeException String                
  deriving (Typeable)

instance Exception RuntimeException

instance Show RuntimeException where
  show (RuntimeException s) = s

err :: String -> TheMonad a
err = io . throw . RuntimeException

toIO :: TheMonad a -> IO a
toIO = id

stats :: TheMonad a -> IO Stats
stats = undefined

statRed :: TheMonad ()
statRed = undefined
