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
import Data.Monoid

data Stats = Stats
  {
    reds :: Int
  }

instance Monoid Stats where
  mempty = Stats 0
  mappend (Stats n) (Stats m) = Stats (n + m)

type TheMonad = WriterT Stats IO

io :: IO a -> TheMonad a
io = lift

stats :: TheMonad a -> IO Stats
stats = execWriterT

statRed :: TheMonad ()
statRed = tell (Stats 1)

